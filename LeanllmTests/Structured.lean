import Leanllm
import LeanllmTests.Baseline
import LeanllmTests.Golden

/-!
Offline tests for structured output (theoriclabs/leanllm#2), run by `lake test`. No network, no API keys: each case runs
`completionAsVia` with a fake transport that encodes the real request with `Codec`, records it,
and decodes a recorded provider response.
-/

open Lean Leanllm

namespace LeanllmTests.Structured

structure Person where
  name : String
  age  : Nat
  deriving FromJson, ToJson, BEq, Repr

def ada : Person := { name := "Ada", age := 36 }

def personSchema : Json := Json.mkObj [
  ("type", "object"),
  ("properties", Json.mkObj [
    ("name", Json.mkObj [("type", "string")]),
    ("age", Json.mkObj [("type", "integer")])
  ]),
  ("required", toJson ["name", "age"]),
  ("additionalProperties", toJson false)
]

def msgs : List Leanllm.Message := [Leanllm.Message.user "Who wrote the Analytical Engine notes, and at what age?"]

/-- Parse a JSON literal used as a recorded response. -/
def pj (s : String) : Json :=
  match Json.parse s with
  | .ok j    => j
  | .error e => panic! s!"bad test JSON: {e}"

/-- Follow object keys (and numeric array indices) through `j`. -/
def path? (j : Json) : List String → Option Json
  | [] => some j
  | k :: ks =>
    match k.toNat?, j with
    | some i, .arr a => a[i]? >>= (path? · ks)
    | _, _ => (j.getObjVal? k).toOption >>= (path? · ks)

/-! ## Harness -/

structure Results where
  passed : Nat := 0
  failed : List String := []

abbrev TestM := StateT Results IO

def check (name : String) (cond : Bool) (detail : String := "") : TestM Unit := do
  if cond then
    modify fun r => { r with passed := r.passed + 1 }
  else
    IO.eprintln s!"FAIL {name}{if detail.isEmpty then "" else s!": {detail}"}"
    modify fun r => { r with failed := r.failed ++ [name] }

def checkPath (name : String) (j : Json) (p : List String) (expected : Json) : TestM Unit :=
  let got := path? j p
  check s!"{name}: {String.intercalate "." p}" (got == some expected)
    s!"expected {expected.compress}, got {(got.map Json.compress).getD "<missing>"}"

def checkAbsent (name : String) (j : Json) (p : List String) : TestM Unit :=
  check s!"{name}: no {String.intercalate "." p}" (path? j p).isNone

/-- A transport that encodes the request with `auth`, stores it, and decodes `recorded`. -/
def fakeCall (auth : Option ResolvedAuth) (recorded : Json) (seen : IO.Ref Json) :
    ModelConfig → List Leanllm.Message → ExceptT LLMError IO LLMResponse := fun cfg ms => do
  let payload ← ExceptT.mk (pure (Codec.encodeRequestWithAuth cfg ms auth))
  seen.set payload
  ExceptT.mk (pure (Codec.decodeResponseWithAuth cfg recorded auth))

/-- Run `completionAsVia` against a recorded response; return the result and the encoded request. -/
def runCase (cfg : ModelConfig) (recorded : Json) (auth : Option ResolvedAuth := none) :
    IO (Except StructuredError (Person × LLMResponse) × Json) := do
  let seen ← IO.mkRef Json.null
  let res ← (completionAsVia (β := Person) (fakeCall auth recorded seen) cfg msgs personSchema).run
  return (res, ← seen.get)

def checkDecoded (name : String) (res : Except StructuredError (Person × LLMResponse)) : TestM Unit :=
  match res with
  | .ok (p, _) => check s!"{name}: decodes Person" (p == ada) s!"got {repr p}"
  | .error e   => check s!"{name}: decodes Person" false s!"error {repr e}"

/-- The first message's text, where the schema instruction goes for prompt strategies. -/
def systemText (payload : Json) (p : List String) : String :=
  ((path? payload p).bind fun j => (j.getStr?).toOption).getD ""

def creds : OAuthCredentials := { accessToken := "test-token" }

/-! ## Recorded responses (shapes from each provider's API reference) -/

def adaText : String := "{\"name\":\"Ada\",\"age\":36}"

def anthropicText (t : String) : Json := Json.mkObj [
  ("id", "msg_01"), ("type", "message"), ("role", "assistant"),
  ("content", toJson [Json.mkObj [("type", "text"), ("text", toJson t)]]),
  ("stop_reason", "end_turn"),
  ("usage", Json.mkObj [("input_tokens", toJson 20), ("output_tokens", toJson 9)])
]

def anthropicToolUse (input : Json) : Json := Json.mkObj [
  ("id", "msg_02"), ("type", "message"), ("role", "assistant"),
  ("content", toJson [Json.mkObj [("type", "tool_use"), ("id", "toolu_01"), ("name", "result"), ("input", input)]]),
  ("stop_reason", "tool_use")
]

def openaiChatText (t : String) : Json := Json.mkObj [
  ("id", "chatcmpl-1"), ("object", "chat.completion"),
  ("choices", toJson [Json.mkObj [
    ("index", toJson 0),
    ("message", Json.mkObj [("role", "assistant"), ("content", toJson t)]),
    ("finish_reason", "stop")
  ]]),
  ("usage", Json.mkObj [("prompt_tokens", toJson 20), ("completion_tokens", toJson 9), ("total_tokens", toJson 29)])
]

def responsesText (t : String) : Json := Json.mkObj [
  ("id", "resp_1"), ("object", "response"), ("status", "completed"),
  ("output", toJson [Json.mkObj [
    ("type", "message"), ("role", "assistant"),
    ("content", toJson [Json.mkObj [("type", "output_text"), ("text", toJson t)]])
  ]])
]

def geminiText (t : String) : Json := Json.mkObj [
  ("candidates", toJson [Json.mkObj [
    ("content", Json.mkObj [("role", "model"), ("parts", toJson [Json.mkObj [("text", toJson t)]])]),
    ("finishReason", "STOP")
  ]])
]

def converseText (t : String) : Json := Json.mkObj [
  ("output", Json.mkObj [("message", Json.mkObj [
    ("role", "assistant"), ("content", toJson [Json.mkObj [("text", toJson t)]])])]),
  ("stopReason", "end_turn")
]

def converseToolUse (input : Json) : Json := Json.mkObj [
  ("output", Json.mkObj [("message", Json.mkObj [
    ("role", "assistant"),
    ("content", toJson [Json.mkObj [("toolUse", Json.mkObj [
      ("toolUseId", "tooluse_1"), ("name", "result"), ("input", input)])]])])]),
  ("stopReason", "tool_use")
]

/-! ## Per-wire-API cases -/

def anthropicTests : TestM Unit := do
  -- Native `output_config.format` for a known Claude model.
  let (res, req) ← runCase { model := .claude .sonnet5_5 } (anthropicText adaText)
  checkDecoded "anthropic native" res
  checkPath "anthropic native" req ["output_config", "format", "type"] "json_schema"
  checkPath "anthropic native" req ["output_config", "format", "schema"] personSchema
  checkAbsent "anthropic native" req ["tool_choice"]
  checkAbsent "anthropic native" req ["system"]

  -- Forced tool for a custom model id.
  let (res, req) ← runCase { model := .claude (.custom "claude-legacy-x") } (anthropicToolUse (toJson ada))
  checkDecoded "anthropic forced tool" res
  checkPath "anthropic forced tool" req ["tool_choice"] (Json.mkObj [("type", "tool"), ("name", "result")])
  checkPath "anthropic forced tool" req ["tools", "0", "name"] "result"
  checkPath "anthropic forced tool" req ["tools", "0", "input_schema"] personSchema

  -- Forced tool keeps the caller's own tools.
  let (_, req) ← runCase { model := .claude (.custom "claude-legacy-x"), tools := [Baseline.weatherTool] }
    (anthropicToolUse (toJson ada))
  checkPath "anthropic forced tool + tools" req ["tools", "0", "name"] "get_weather"
  checkPath "anthropic forced tool + tools" req ["tools", "1", "name"] "result"

  -- Claude subscription OAuth envelope.
  let (res, req) ← runCase { model := .claude .opus5_5 } (anthropicText adaText) (some (.claudeOAuth creds))
  checkDecoded "anthropic oauth" res
  checkPath "anthropic oauth" req ["output_config", "format", "type"] "json_schema"

  -- Bedrock InvokeModel uses the Anthropic body.
  let (res, req) ← runCase { model := .claude .haiku4_5, provider := .bedrock {}, wireApi := .bedrockInvoke }
    (anthropicText adaText)
  checkDecoded "bedrock invoke" res
  checkPath "bedrock invoke" req ["anthropic_version"] "bedrock-2023-05-31"
  checkPath "bedrock invoke" req ["output_config", "format", "schema"] personSchema

def bedrockConverseTests : TestM Unit := do
  let (res, req) ← runCase { model := .claude .haiku4_5, provider := .bedrock {} } (converseText adaText)
  checkDecoded "bedrock converse native" res
  checkPath "bedrock converse native" req ["outputConfig", "textFormat", "type"] "json_schema"
  checkPath "bedrock converse native" req ["outputConfig", "textFormat", "structure", "jsonSchema", "schema"]
    (toJson personSchema.compress)
  checkPath "bedrock converse native" req ["outputConfig", "textFormat", "structure", "jsonSchema", "name"] "result"

  let (res, req) ← runCase
    { model := .claude (.custom "anthropic.claude-legacy"), provider := .bedrock {}, tools := [Baseline.weatherTool] }
    (converseToolUse (toJson ada))
  checkDecoded "bedrock converse forced tool" res
  checkPath "bedrock converse forced tool" req ["toolConfig", "toolChoice", "tool", "name"] "result"
  checkPath "bedrock converse forced tool" req ["toolConfig", "tools", "0", "toolSpec", "name"] "get_weather"
  checkPath "bedrock converse forced tool" req ["toolConfig", "tools", "1", "toolSpec", "inputSchema", "json"] personSchema

def openaiChatTests : TestM Unit := do
  let (res, req) ← runCase { model := .openai .gpt5_4 } (openaiChatText adaText)
  checkDecoded "openai chat" res
  checkPath "openai chat" req ["response_format", "type"] "json_schema"
  checkPath "openai chat" req ["response_format", "json_schema", "name"] "result"
  checkPath "openai chat" req ["response_format", "json_schema", "strict"] (toJson true)
  checkPath "openai chat" req ["response_format", "json_schema", "schema"] personSchema
  check "openai chat: no schema prompt"
    (((req.getObjValAs? (Array Json) "messages").toOption.map (·.size)) == some 1)

  -- DeepSeek documents only JSON mode: json_object plus a prompt that says "JSON".
  let (res, req) ← runCase { model := .deepseek .v4Flash } (openaiChatText adaText)
  checkDecoded "deepseek json_object" res
  checkPath "deepseek json_object" req ["response_format"] (Json.mkObj [("type", "json_object")])
  checkPath "deepseek json_object" req ["messages", "0", "role"] "system"
  check "deepseek json_object: prompt carries schema"
    ((systemText req ["messages", "0", "content"]).contains personSchema.compress)

  -- MiniMax documents no response_format: prompt only, fenced reply with prose around it.
  let fenced := "Here you go:\n```json\n{\"name\": \"Ada\", \"age\": 36}\n```\nAnything else?"
  let (res, req) ← runCase { model := .minimax (.custom "MiniMax-M3") } (openaiChatText fenced)
  checkDecoded "minimax fenced fallback" res
  checkAbsent "minimax fenced fallback" req ["response_format"]
  check "minimax fenced fallback: prompt carries schema"
    ((systemText req ["messages", "0", "content"]).contains "JSON Schema")

  -- OpenRouter passes json_schema through.
  let (_, req) ← runCase { model := .deepseek .v4Flash, provider := .openRouter {} } (openaiChatText adaText)
  checkPath "openrouter" req ["response_format", "type"] "json_schema"

def openaiResponsesTests : TestM Unit := do
  let (res, req) ← runCase { model := .openai .gpt5_4, wireApi := .openaiResponses } (responsesText adaText)
  checkDecoded "openai responses" res
  checkPath "openai responses" req ["text", "format", "type"] "json_schema"
  checkPath "openai responses" req ["text", "format", "name"] "result"
  checkPath "openai responses" req ["text", "format", "strict"] (toJson true)
  checkPath "openai responses" req ["text", "format", "schema"] personSchema

  -- ChatGPT/Codex OAuth backend (`stream: true` envelope) takes the same `text.format`.
  let (res, req) ← runCase { model := .openai .gpt5_4 } (responsesText adaText) (some (.openaiOAuth creds))
  checkDecoded "chatgpt oauth responses" res
  checkPath "chatgpt oauth responses" req ["stream"] (toJson true)
  checkPath "chatgpt oauth responses" req ["text", "format", "schema"] personSchema

def geminiTests : TestM Unit := do
  let (res, req) ← runCase { model := .gemini .flash3_5, maxTokens := some 64 } (geminiText adaText)
  checkDecoded "gemini" res
  checkPath "gemini" req ["generationConfig", "responseMimeType"] "application/json"
  checkPath "gemini" req ["generationConfig", "responseJsonSchema"] personSchema
  checkPath "gemini" req ["generationConfig", "maxOutputTokens"] (toJson 64)

  -- Code Assist (`v1internal`) envelope: fields go inside `request`, reply is wrapped in `response`.
  let (res, req) ← runCase { model := .gemini .flash3_5 }
    (Json.mkObj [("response", geminiText adaText)]) (some (.geminiOAuth creds "proj-1"))
  checkDecoded "gemini code assist" res
  checkPath "gemini code assist" req ["request", "generationConfig", "responseJsonSchema"] personSchema
  checkPath "gemini code assist" req ["project"] "proj-1"
  checkAbsent "gemini code assist" req ["generationConfig"]

/-! ## Errors -/

def errorTests : TestM Unit := do
  let isViolation (name raw : String) (res : Except StructuredError (Person × LLMResponse)) : TestM Unit :=
    match res with
    | .error (.schemaViolation expected r err) =>
      do
        check s!"{name}: raw text kept" (r == raw) s!"got {r}"
        check s!"{name}: decode error set" (!err.isEmpty)
        check s!"{name}: expected names schema" (expected.contains personSchema.compress)
    | other => check s!"{name}: schemaViolation" false s!"got {repr (other.map (·.1))}"

  let missingAge := "{\"name\":\"Ada\"}"
  let (res, _) ← runCase { model := .openai .gpt5_4 } (openaiChatText missingAge)
  isViolation "violation missing field" missingAge res

  let prose := "I'd rather not say."
  let (res, _) ← runCase { model := .claude .sonnet5_5 } (anthropicText prose)
  isViolation "violation no json" prose res

  let badInput := Json.mkObj [("name", toJson 3)]
  let (res, _) ← runCase { model := .claude (.custom "claude-legacy-x") } (anthropicToolUse badInput)
  isViolation "violation forced tool input" badInput.compress res

  let (res, _) ← runCase { model := .gemini .flash3_5 } (geminiText "{\"name\":\"Ada\",\"age\":-1}")
  isViolation "violation gemini" "{\"name\":\"Ada\",\"age\":-1}" res

  -- LLM failures pass through as `.llm`.
  let failing : ModelConfig → List Leanllm.Message → ExceptT LLMError IO LLMResponse :=
    fun _ _ => throw (.rateLimit .quotaExceeded "quota")
  let res ← (completionAsVia (β := Person) failing { model := .openai .gpt5_4 } msgs personSchema).run
  check "llm error passes through" (match res with
    | .error (.llm (.rateLimit .quotaExceeded _)) => true
    | _ => false)

/-! ## Unchanged encoding without a response format -/

def baselineTests : TestM Unit := do
  for (label, cfg, auth) in Baseline.cases do
    let now := Baseline.encode cfg auth
    match Golden.encodings.lookup label with
    | some before => check s!"unchanged encoding: {label}" (now == before) s!"now {now}"
    | none        => check s!"unchanged encoding: {label}" false "no golden entry"

/-! ## Helpers -/

def helperTests : TestM Unit := do
  let ex (s : String) := (Structured.extractJson? s).map Json.compress
  check "extract: plain" (ex adaText == some (pj adaText).compress)
  check "extract: json fence" (ex "```json\n{\"a\":1}\n```" == some "{\"a\":1}")
  check "extract: bare fence" (ex "```\n{\"a\":1}\n```" == some "{\"a\":1}")
  check "extract: skips non-JSON braces" (ex "Use {placeholder} like this: {\"a\":1} ok" == some "{\"a\":1}")
  check "extract: braces inside strings" (ex "x {\"a\":\"}{\\\"\"} y" == some "{\"a\":\"}{\\\"\"}")
  check "extract: nested" (ex "-> {\"a\":{\"b\":[1,{}]}} <-" == some "{\"a\":{\"b\":[1,{}]}}")
  check "extract: none" (ex "no json here" == none)

  -- Strategy table spot checks.
  let s (api : WireApi) (p : Provider) (m : Model) := ResponseFormat.strategyFor api p m
  check "strategy: qwen3.7-plus native" (s .openaiChat .direct (.qwen .qwen3_7Plus) == .nativeSchema)
  check "strategy: qwen3.5-plus json_object" (s .openaiChat .direct (.qwen .qwen3_5Plus) == .jsonObject)
  check "strategy: glm json_object" (s .openaiChat .direct (.glm (.custom "glm-5")) == .jsonObject)
  check "strategy: kimi native" (s .openaiChat .direct (.kimi .k2_6) == .nativeSchema)
  check "strategy: grok native" (s .openaiChat .direct (.grok .grok4_7) == .nativeSchema)
  check "strategy: mistral native" (s .openaiChat .direct (.mistral (.custom "mistral-large-latest")) == .nativeSchema)
  check "strategy: azure native" (s .openaiChat (.azure { endpoint := "https://x" }) (.openai .gpt5_4) == .nativeSchema)
  check "strategy: groq prompt" (s .openaiChat .groq (.openai (.custom "gpt-oss-120b")) == .promptOnly)
  check "strategy: local prompt" (s .openaiChat .ollama (.qwen (.custom "qwen2.5")) == .promptOnly)

  -- The instruction joins an existing leading system message instead of adding another.
  let rf : ResponseFormat := { schema := personSchema }
  let withSys := Structured.withInstruction rf [Leanllm.Message.system "Be terse.", Leanllm.Message.user "hi"]
  check "instruction merges into system message" (withSys.length == 2 && (withSys.head?.map (·.content.length)) == some 2)

def all : TestM Unit := do
  anthropicTests
  bedrockConverseTests
  openaiChatTests
  openaiResponsesTests
  geminiTests
  errorTests
  baselineTests
  helperTests

/-- Run the structured-output suite; returns `(passed, failed names)`. -/
def run : IO (Nat × List String) := do
  let ((), r) ← all.run {}
  return (r.passed, r.failed)

end LeanllmTests.Structured
