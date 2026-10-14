import Lean.Data.Json
import Leanllm.Provider
import Leanllm.Models
import Leanllm.Message
import Leanllm.Error
import Leanllm.Response

namespace Leanllm.Codec

open Lean

private def optField [ToJson α] (key : String) : Option α → List (String × Json)
  | some v => [(key, toJson v)]
  | none   => []

private def listField [ToJson α] (key : String) (xs : List α) : List (String × Json) :=
  if xs.isEmpty then [] else [(key, toJson xs)]

private def getStr? (j : Json) (key : String) : Option String :=
  (j.getObjValAs? String key).toOption

private def getNat? (j : Json) (key : String) : Option Nat :=
  (j.getObjValAs? Nat key).toOption

private def getArr? (j : Json) (key : String) : Option (Array Json) :=
  (j.getObjValAs? (Array Json) key).toOption

private def getObj? (j : Json) (key : String) : Option Json :=
  (j.getObjVal? key).toOption

private def isSystemRole : Role → Bool
  | .system | .developer => true
  | _ => false

private def extractSystemText (messages : List Message) : Option String :=
  let texts := messages.filter (fun m => isSystemRole m.role) |>.flatMap fun m =>
    m.content.filterMap fun
      | .text t => some t
      | _       => none
  if texts.isEmpty then none else some (String.intercalate "\n\n" texts)

/-! ## Anthropic Messages Wire Codec (`/v1/messages` & Bedrock Invoke) -/

private def encodeAnthropicPart : ContentPart → Json
  | .text t =>
    Json.mkObj [("type", "text"), ("text", t)]
  | .image (.url u) =>
    Json.mkObj [
      ("type", "image"),
      ("source", Json.mkObj [("type", "url"), ("url", u)])
    ]
  | .image (.base64 mediaType data) =>
    Json.mkObj [
      ("type", "image"),
      ("source", Json.mkObj [
        ("type", "base64"),
        ("media_type", mediaType),
        ("data", data)
      ])
    ]
  | .toolCall tc =>
    Json.mkObj [
      ("type", "tool_use"),
      ("id", tc.id),
      ("name", tc.name),
      ("input", tc.arguments)
    ]
  | .toolResult tr =>
    Json.mkObj [
      ("type", "tool_result"),
      ("tool_use_id", tr.toolCallId),
      ("content", tr.content),
      ("is_error", toJson tr.isError)
    ]
  | .thinking thought sig =>
    Json.mkObj ([("type", toJson "thinking"), ("thinking", toJson thought)] ++ optField "signature" sig)

private def encodeAnthropicMessage (msg : Message) : Option Json :=
  if isSystemRole msg.role then none
  else
    let roleStr := match msg.role with
      | .assistant => "assistant"
      | _          => "user"
    let parts := msg.content.map encodeAnthropicPart
    some <| Json.mkObj [
      ("role", toJson roleStr),
      ("content", toJson parts)
    ]

private def encodeAnthropicTool (t : ToolSpec) : Json :=
  Json.mkObj [
    ("name", toJson t.name),
    ("description", toJson t.description),
    ("input_schema", t.parameters)
  ]

def encodeAnthropicMessages (cfg : ModelConfig) (messages : List Message) (forBedrockInvoke : Bool := false) : Json :=
  let sys := extractSystemText messages
  let msgObjs := messages.filterMap encodeAnthropicMessage
  let toolObjs := cfg.tools.map encodeAnthropicTool
  let headerFields : List (String × Json) :=
    if forBedrockInvoke then
      [("anthropic_version", toJson "bedrock-2023-05-31")]
    else
      [("model", toJson cfg.effectiveModelId)]
  Json.mkObj <|
    headerFields ++
    [("max_tokens", toJson (cfg.maxTokens.getD 4096)),
     ("messages", toJson msgObjs)] ++
    optField "system" sys ++
    optField "temperature" cfg.temperature ++
    optField "top_p" cfg.topP ++
    listField "stop_sequences" cfg.stop ++
    listField "tools" toolObjs

private def decodeAnthropicPart (j : Json) : Option ContentPart := do
  let kind ← getStr? j "type"
  match kind with
  | "text" =>
    let t ← getStr? j "text"
    some (.text t)
  | "thinking" =>
    let t ← getStr? j "thinking"
    let sig := getStr? j "signature"
    some (.thinking t sig)
  | "tool_use" =>
    let id ← getStr? j "id"
    let name ← getStr? j "name"
    let input := (getObj? j "input").getD (Json.mkObj [])
    some (.toolCall { id, name, arguments := input })
  | _ => none

def decodeAnthropicMessages (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse := do
  let some contentArr := getArr? j "content"
    | throw (.parse (.unexpectedSchema "Anthropic response with 'content' array" j) "Missing 'content' array")
  let parts := contentArr.toList.filterMap decodeAnthropicPart
  let id := (getStr? j "id").getD ""
  let stopReason := match getStr? j "stop_reason" with
    | some "end_turn"      => StopReason.endTurn
    | some "max_tokens"    => StopReason.maxTokens
    | some "stop_sequence" => StopReason.stopSequence (getStr? j "stop_sequence")
    | some "tool_use"      => StopReason.toolUse
    | some "refusal"       => StopReason.contentFilter
    | some other           => StopReason.other other
    | none                 => StopReason.endTurn
  let usage := do
    let u ← getObj? j "usage"
    let inputTokens ← getNat? u "input_tokens"
    let outputTokens ← getNat? u "output_tokens"
    pure {
      inputTokens
      outputTokens
      cacheReadTokens  := getNat? u "cache_read_input_tokens"
      cacheWriteTokens := getNat? u "cache_creation_input_tokens"
    }
  return {
    id
    model   := cfg.model
    message := { role := .assistant, content := parts }
    stopReason
    usage
  }

/-! ## OpenAI Chat Completions Wire Codec (`/v1/chat/completions` & OpenRouter & Azure) -/

private def roleToOpenAI : Role → String
  | .system    => "system"
  | .developer => "developer"
  | .user      => "user"
  | .assistant => "assistant"
  | .tool      => "tool"

private def encodeOpenAIMessage (msg : Message) : List Json :=
  let toolResults := msg.content.filterMap fun
    | .toolResult tr => some tr
    | _              => none
  if !toolResults.isEmpty then
    toolResults.map fun tr =>
      Json.mkObj [
        ("role", "tool"),
        ("tool_call_id", toJson tr.toolCallId),
        ("content", toJson tr.content)
      ]
  else
    let toolCalls := msg.content.filterMap fun
      | .toolCall tc => some <| Json.mkObj [
          ("id", toJson tc.id),
          ("type", "function"),
          ("function", Json.mkObj [
            ("name", toJson tc.name),
            ("arguments", toJson tc.arguments.compress)
          ])
        ]
      | _ => none
    let hasImages := msg.content.any fun
      | .image _ => true
      | _        => false
    let contentField : List (String × Json) :=
      if hasImages then
        let parts := msg.content.filterMap fun
          | .text t => some <| Json.mkObj [("type", "text"), ("text", toJson t)]
          | .image (.url u) => some <| Json.mkObj [
              ("type", "image_url"),
              ("image_url", Json.mkObj [("url", toJson u)])
            ]
          | .image (.base64 mime data) => some <| Json.mkObj [
              ("type", "image_url"),
              ("image_url", Json.mkObj [("url", toJson s!"data:{mime};base64,{data}")])
            ]
          | _ => none
        [("content", toJson parts)]
      else
        let texts := msg.content.filterMap fun
          | .text t => some t
          | _       => none
        if texts.isEmpty && !toolCalls.isEmpty then
          [("content", Json.null)]
        else
          [("content", toJson (String.intercalate "\n" texts))]
    [Json.mkObj <|
      [("role", toJson (roleToOpenAI msg.role))] ++
      contentField ++
      optField "name" msg.name ++
      listField "tool_calls" toolCalls]

private def encodeOpenAITool (t : ToolSpec) : Json :=
  Json.mkObj [
    ("type", "function"),
    ("function", Json.mkObj [
      ("name", toJson t.name),
      ("description", toJson t.description),
      ("parameters", t.parameters)
    ])
  ]

private def encodeOpenRouterExtra (provider : Provider) (primaryModelId : String) : List (String × Json) :=
  match provider with
  | .openRouter orCfg =>
    let modelsField :=
      if orCfg.fallbacks.isEmpty then []
      else
        let allModels := primaryModelId :: orCfg.fallbacks.map (provider.resolveModelId ·)
        [("models", toJson allModels)]
    let sortStr : Option String := orCfg.sort.map fun
      | .price      => "price"
      | .throughput => "throughput"
      | .latency    => "latency"
    let providerSubfields :=
      listField "order" orCfg.order ++
      (if orCfg.allowFallbacks then [] else [("allow_fallbacks", toJson false)]) ++
      optField "sort" sortStr
    let providerField :=
      if providerSubfields.isEmpty then []
      else [("provider", Json.mkObj providerSubfields)]
    modelsField ++ providerField
  | _ => []

def encodeOpenAIChat (cfg : ModelConfig) (messages : List Message) : Json :=
  let msgObjs := messages.flatMap encodeOpenAIMessage
  let toolObjs := cfg.tools.map encodeOpenAITool
  let modelId := cfg.effectiveModelId
  Json.mkObj <|
    [("model", toJson modelId),
     ("messages", toJson msgObjs)] ++
    optField "temperature" cfg.temperature ++
    optField "max_tokens" cfg.maxTokens ++
    optField "top_p" cfg.topP ++
    listField "stop" cfg.stop ++
    listField "tools" toolObjs ++
    encodeOpenRouterExtra cfg.provider modelId

private def decodeOpenAIToolCall (j : Json) : Except LLMError ContentPart := do
  let id := (getStr? j "id").getD ""
  let some fn := getObj? j "function"
    | throw (.parse (.unexpectedSchema "tool_call with 'function' object" j) "Missing 'function' in tool_call")
  let some name := getStr? fn "name"
    | throw (.parse (.unexpectedSchema "function with 'name'" fn) "Missing tool name")
  let rawArgs := (getStr? fn "arguments").getD "{}"
  let args ← match Json.parse rawArgs with
    | .ok parsed => pure parsed
    | .error _   => throw (.parse (.invalidToolArgs name rawArgs) s!"Invalid JSON arguments for tool '{name}'")
  return .toolCall { id, name, arguments := args }

def decodeOpenAIChat (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse := do
  if let some errObj := getObj? j "error" then
    let msg := (getStr? errObj "message").getD errObj.compress
    throw (.provider (.statusCode 500 msg) msg)
  let some choices := getArr? j "choices"
    | throw (.parse (.unexpectedSchema "OpenAI response with 'choices' array" j) "Missing 'choices' array")
  let some firstChoice := choices[0]?
    | throw (.parse (.unexpectedSchema "Non-empty 'choices' array" j) "Empty 'choices' array")
  let some msgObj := getObj? firstChoice "message"
    | throw (.parse (.unexpectedSchema "Choice with 'message' object" firstChoice) "Missing 'message' in choice")

  let mut parts : List ContentPart := []
  if let some reasoning := (getStr? msgObj "reasoning_content").or (getStr? msgObj "reasoning") then
    if !reasoning.isEmpty then
      parts := parts ++ [.thinking reasoning none]
  if let some text := getStr? msgObj "content" then
    if !text.isEmpty then
      parts := parts ++ [.text text]
  if let some tcArr := getArr? msgObj "tool_calls" then
    for tcJson in tcArr do
      let tcPart ← decodeOpenAIToolCall tcJson
      parts := parts ++ [tcPart]

  let stopReason := match getStr? firstChoice "finish_reason" with
    | some "stop"           => StopReason.endTurn
    | some "length"         => StopReason.maxTokens
    | some "tool_calls"     => StopReason.toolUse
    | some "function_call"  => StopReason.toolUse
    | some "content_filter" => StopReason.contentFilter
    | some other            => StopReason.other other
    | none                  => StopReason.endTurn

  let usage := do
    let u ← getObj? j "usage"
    let inputTokens ← getNat? u "prompt_tokens"
    let outputTokens ← getNat? u "completion_tokens"
    let cacheRead := (getObj? u "prompt_tokens_details") >>= (getNat? · "cached_tokens")
    let reasoning := (getObj? u "completion_tokens_details") >>= (getNat? · "reasoning_tokens")
    pure {
      inputTokens
      outputTokens
      cacheReadTokens := cacheRead
      reasoningTokens := reasoning
    }

  return {
    id      := (getStr? j "id").getD ""
    model   := cfg.model
    message := { role := .assistant, content := parts }
    stopReason
    usage
  }

/-! ## OpenAI Responses Wire Codec (`/v1/responses`) -/

private def encodeResponsesInput (msg : Message) : List Json :=
  if isSystemRole msg.role then []
  else
    msg.content.filterMap fun
      | .text t =>
        let roleStr := if msg.role == .assistant then "assistant" else "user"
        some <| Json.mkObj [
          ("type", "message"),
          ("role", toJson roleStr),
          ("content", toJson t)
        ]
      | .toolCall tc =>
        some <| Json.mkObj [
          ("type", "function_call"),
          ("call_id", toJson tc.id),
          ("name", toJson tc.name),
          ("arguments", toJson tc.arguments.compress)
        ]
      | .toolResult tr =>
        some <| Json.mkObj [
          ("type", "function_call_output"),
          ("call_id", toJson tr.toolCallId),
          ("output", toJson tr.content)
        ]
      | _ => none

private def encodeResponsesTool (t : ToolSpec) : Json :=
  Json.mkObj [
    ("type", "function"),
    ("name", toJson t.name),
    ("description", toJson t.description),
    ("parameters", t.parameters)
  ]

def encodeOpenAIResponses (cfg : ModelConfig) (messages : List Message) : Json :=
  let instructions := extractSystemText messages
  let inputs := messages.flatMap encodeResponsesInput
  let tools := cfg.tools.map encodeResponsesTool
  Json.mkObj <|
    [("model", toJson cfg.effectiveModelId),
     ("input", toJson inputs)] ++
    optField "instructions" instructions ++
    optField "temperature" cfg.temperature ++
    optField "max_output_tokens" cfg.maxTokens ++
    optField "top_p" cfg.topP ++
    listField "tools" tools

def decodeOpenAIResponses (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse := do
  let some outputArr := getArr? j "output"
    | throw (.parse (.unexpectedSchema "Responses payload with 'output' array" j) "Missing 'output' array")
  let mut parts : List ContentPart := []
  let mut hasToolCall := false
  for item in outputArr do
    match getStr? item "type" with
    | some "message" =>
      if let some contentArr := getArr? item "content" then
        for c in contentArr do
          if let some t := getStr? c "text" then
            parts := parts ++ [.text t]
    | some "function_call" =>
      hasToolCall := true
      let callId := (getStr? item "call_id").or (getStr? item "id") |>.getD ""
      let name := (getStr? item "name").getD ""
      let rawArgs := (getStr? item "arguments").getD "{}"
      let args ← match Json.parse rawArgs with
        | .ok parsed => pure parsed
        | .error _   => throw (.parse (.invalidToolArgs name rawArgs) s!"Invalid JSON arguments for tool '{name}'")
      parts := parts ++ [.toolCall { id := callId, name, arguments := args }]
    | some "reasoning" =>
      if let some summaryArr := getArr? item "summary" then
        for s in summaryArr do
          if let some t := getStr? s "text" then
            parts := parts ++ [.thinking t none]
    | _ => pure ()

  let stopReason :=
    if hasToolCall then StopReason.toolUse
    else match getStr? j "status" with
      | some "completed"  => StopReason.endTurn
      | some "incomplete" => StopReason.maxTokens
      | some other        => StopReason.other other
      | none              => StopReason.endTurn

  let usage := do
    let u ← getObj? j "usage"
    let inputTokens ← getNat? u "input_tokens"
    let outputTokens ← getNat? u "output_tokens"
    pure { inputTokens, outputTokens }

  return {
    id      := (getStr? j "id").getD ""
    model   := cfg.model
    message := { role := .assistant, content := parts }
    stopReason
    usage
  }

/-! ## Google Gemini Wire Codec (`:generateContent`) -/

private def encodeGeminiPart : ContentPart → Option Json
  | .text t => some <| Json.mkObj [("text", toJson t)]
  | .image (.base64 mime data) =>
    some <| Json.mkObj [
      ("inlineData", Json.mkObj [("mimeType", toJson mime), ("data", toJson data)])
    ]
  | .image (.url u) =>
    some <| Json.mkObj [
      ("fileData", Json.mkObj [("fileUri", toJson u)])
    ]
  | .toolCall tc =>
    some <| Json.mkObj [
      ("functionCall", Json.mkObj [("name", toJson tc.name), ("args", tc.arguments)])
    ]
  | .toolResult tr =>
    some <| Json.mkObj [
      ("functionResponse", Json.mkObj [
        ("name", toJson tr.toolCallId),
        ("response", Json.mkObj [("result", toJson tr.content), ("is_error", toJson tr.isError)])
      ])
    ]
  | .thinking _ _ => none

private def encodeGeminiMessage (msg : Message) : Option Json :=
  if isSystemRole msg.role then none
  else
    let roleStr := match msg.role with
      | .assistant => "model"
      | _          => "user"
    let parts := msg.content.filterMap encodeGeminiPart
    if parts.isEmpty then none
    else some <| Json.mkObj [("role", toJson roleStr), ("parts", toJson parts)]

def encodeGeminiGenerate (cfg : ModelConfig) (messages : List Message) : Json :=
  let sysField : List (String × Json) :=
    match extractSystemText messages with
    | some sys => [("systemInstruction", Json.mkObj [("parts", toJson [Json.mkObj [("text", toJson sys)]])])]
    | none     => []
  let contents := messages.filterMap encodeGeminiMessage
  let toolsField : List (String × Json) :=
    if cfg.tools.isEmpty then []
    else
      let decls := cfg.tools.map fun t =>
        Json.mkObj [
          ("name", toJson t.name),
          ("description", toJson t.description),
          ("parameters", t.parameters)
        ]
      [("tools", toJson [Json.mkObj [("functionDeclarations", toJson decls)]])]
  let genCfgFields :=
    optField "temperature" cfg.temperature ++
    optField "maxOutputTokens" cfg.maxTokens ++
    optField "topP" cfg.topP ++
    listField "stopSequences" cfg.stop
  let genCfg :=
    if genCfgFields.isEmpty then []
    else [("generationConfig", Json.mkObj genCfgFields)]
  Json.mkObj (sysField ++ [("contents", toJson contents)] ++ toolsField ++ genCfg)

def decodeGeminiGenerate (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse := do
  let some candidates := getArr? j "candidates"
    | throw (.parse (.unexpectedSchema "Gemini response with 'candidates' array" j) "Missing 'candidates' array")
  let some first := candidates[0]?
    | throw (.parse (.unexpectedSchema "Non-empty 'candidates' array" j) "Empty 'candidates' array")
  let mut parts : List ContentPart := []
  let mut hasToolCall := false
  if let some contentObj := getObj? first "content" then
    if let some partsArr := getArr? contentObj "parts" then
      for p in partsArr do
        let isThought := (p.getObjValAs? Bool "thought").toOption.getD false
        if let some t := getStr? p "text" then
          if isThought then
            parts := parts ++ [.thinking t none]
          else
            parts := parts ++ [.text t]
        if let some fc := getObj? p "functionCall" then
          hasToolCall := true
          let name := (getStr? fc "name").getD ""
          let args := (getObj? fc "args").getD (Json.mkObj [])
          parts := parts ++ [.toolCall { id := name, name, arguments := args }]

  let stopReason :=
    if hasToolCall then StopReason.toolUse
    else match getStr? first "finishReason" with
      | some "STOP"               => StopReason.endTurn
      | some "MAX_TOKENS"         => StopReason.maxTokens
      | some "SAFETY"             => StopReason.contentFilter
      | some "RECITATION"         => StopReason.contentFilter
      | some "PROHIBITED_CONTENT" => StopReason.contentFilter
      | some other                => StopReason.other other
      | none                      => StopReason.endTurn

  let usage := do
    let u ← getObj? j "usageMetadata"
    let inputTokens ← getNat? u "promptTokenCount"
    let outputTokens := (getNat? u "candidatesTokenCount").getD 0
    pure {
      inputTokens
      outputTokens
      cacheReadTokens := getNat? u "cachedContentTokenCount"
      reasoningTokens := getNat? u "thoughtsTokenCount"
    }

  return {
    id      := (getStr? j "responseId").getD ""
    model   := cfg.model
    message := { role := .assistant, content := parts }
    stopReason
    usage
  }

/-! ## AWS Bedrock Converse Wire Codec (`/model/{modelId}/converse`) -/

private def encodeBedrockPart : ContentPart → Option Json
  | .text t => some <| Json.mkObj [("text", toJson t)]
  | .toolCall tc =>
    some <| Json.mkObj [
      ("toolUse", Json.mkObj [
        ("toolUseId", toJson tc.id),
        ("name", toJson tc.name),
        ("input", tc.arguments)
      ])
    ]
  | .toolResult tr =>
    some <| Json.mkObj [
      ("toolResult", Json.mkObj [
        ("toolUseId", toJson tr.toolCallId),
        ("content", toJson [Json.mkObj [("text", toJson tr.content)]]),
        ("status", toJson (if tr.isError then "error" else "success"))
      ])
    ]
  | _ => none

private def encodeBedrockMessage (msg : Message) : Option Json :=
  if isSystemRole msg.role then none
  else
    let roleStr := match msg.role with
      | .assistant => "assistant"
      | _          => "user"
    let parts := msg.content.filterMap encodeBedrockPart
    if parts.isEmpty then none
    else some <| Json.mkObj [("role", toJson roleStr), ("content", toJson parts)]

def encodeBedrockConverse (cfg : ModelConfig) (messages : List Message) : Json :=
  let sysField : List (String × Json) :=
    match extractSystemText messages with
    | some sys => [("system", toJson [Json.mkObj [("text", toJson sys)]])]
    | none     => []
  let msgObjs := messages.filterMap encodeBedrockMessage
  let infFields :=
    optField "maxTokens" cfg.maxTokens ++
    optField "temperature" cfg.temperature ++
    optField "topP" cfg.topP ++
    listField "stopSequences" cfg.stop
  let infField :=
    if infFields.isEmpty then []
    else [("inferenceConfig", Json.mkObj infFields)]
  let toolField :=
    if cfg.tools.isEmpty then []
    else
      let specs := cfg.tools.map fun t =>
        Json.mkObj [
          ("toolSpec", Json.mkObj [
            ("name", toJson t.name),
            ("description", toJson t.description),
            ("inputSchema", Json.mkObj [("json", t.parameters)])
          ])
        ]
      [("toolConfig", Json.mkObj [("tools", toJson specs)])]
  Json.mkObj ([("messages", toJson msgObjs)] ++ sysField ++ infField ++ toolField)

def decodeBedrockConverse (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse := do
  let some outputObj := getObj? j "output"
    | throw (.parse (.unexpectedSchema "Bedrock Converse response with 'output'" j) "Missing 'output' object")
  let some msgObj := getObj? outputObj "message"
    | throw (.parse (.unexpectedSchema "Bedrock Converse output with 'message'" outputObj) "Missing 'message' object")
  let some contentArr := getArr? msgObj "content"
    | throw (.parse (.unexpectedSchema "Bedrock Converse message with 'content'" msgObj) "Missing 'content' array")

  let mut parts : List ContentPart := []
  for item in contentArr do
    if let some t := getStr? item "text" then
      parts := parts ++ [.text t]
    if let some tu := getObj? item "toolUse" then
      let id := (getStr? tu "toolUseId").getD ""
      let name := (getStr? tu "name").getD ""
      let input := (getObj? tu "input").getD (Json.mkObj [])
      parts := parts ++ [.toolCall { id, name, arguments := input }]
    if let some rc := getObj? item "reasoningContent" then
      if let some rt := getObj? rc "reasoningText" then
        if let some t := getStr? rt "text" then
          parts := parts ++ [.thinking t (getStr? rt "signature")]

  let stopReason := match getStr? j "stopReason" with
    | some "end_turn"             => StopReason.endTurn
    | some "max_tokens"           => StopReason.maxTokens
    | some "stop_sequence"        => StopReason.stopSequence none
    | some "tool_use"             => StopReason.toolUse
    | some "content_filtered"     => StopReason.contentFilter
    | some "guardrail_intervened" => StopReason.contentFilter
    | some other                  => StopReason.other other
    | none                        => StopReason.endTurn

  let usage := do
    let u ← getObj? j "usage"
    let inputTokens ← getNat? u "inputTokens"
    let outputTokens ← getNat? u "outputTokens"
    pure {
      inputTokens
      outputTokens
      cacheReadTokens  := getNat? u "cacheReadInputTokens"
      cacheWriteTokens := getNat? u "cacheWriteInputTokens"
    }

  return {
    id      := ""
    model   := cfg.model
    message := { role := .assistant, content := parts }
    stopReason
    usage
  }

/-! ## Unified Wire Dispatch -/

/-- Encode a `ModelConfig` and `List Message` into the wire JSON for `cfg.effectiveWireApi`. -/
def encodeRequest (cfg : ModelConfig) (messages : List Message) : Except LLMError Json :=
  match cfg.effectiveWireApi with
  | .anthropicMessages => .ok (encodeAnthropicMessages cfg messages false)
  | .openaiChat        => .ok (encodeOpenAIChat cfg messages)
  | .openaiResponses   => .ok (encodeOpenAIResponses cfg messages)
  | .geminiGenerate    => .ok (encodeGeminiGenerate cfg messages)
  | .bedrockConverse   => .ok (encodeBedrockConverse cfg messages)
  | .bedrockInvoke     => .ok (encodeAnthropicMessages cfg messages true)
  | .auto              => .ok (encodeOpenAIChat cfg messages)

/-- Decode a wire JSON payload into a canonical `LLMResponse` using `cfg.effectiveWireApi`. -/
def decodeResponse (cfg : ModelConfig) (j : Json) : Except LLMError LLMResponse :=
  match cfg.effectiveWireApi with
  | .anthropicMessages => decodeAnthropicMessages cfg j
  | .openaiChat        => decodeOpenAIChat cfg j
  | .openaiResponses   => decodeOpenAIResponses cfg j
  | .geminiGenerate    => decodeGeminiGenerate cfg j
  | .bedrockConverse   => decodeBedrockConverse cfg j
  | .bedrockInvoke     => decodeAnthropicMessages cfg j
  | .auto              => decodeOpenAIChat cfg j

end Leanllm.Codec
