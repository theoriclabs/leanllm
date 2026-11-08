import Lean.Data.Json
import Leanllm.Provider
import Leanllm.Message
import Leanllm.Auth.Claude

/-!
# Structured output: request side

`ResponseFormat` asks the model for a JSON value that follows a JSON Schema. When
`ModelConfig.responseFormat` is `none` (the default), nothing here runs and requests encode
exactly as before.

`ResponseFormat.strategyFor` picks how each `(WireApi, Provider, Model)` asks for JSON, and
`ResponseFormat.applyToPayload` adds the matching fields to an already-encoded request body.
Provider documentation used for each field (all read on 2026-11-08):

* OpenAI Chat Completions `response_format: {type: "json_schema", json_schema: {name, description?, schema, strict}}`
  and `{type: "json_object"}`:
  https://developers.openai.com/api/reference/resources/chat/subresources/completions/methods/create
* OpenAI Responses `text.format: {type: "json_schema", name, schema, strict}`:
  https://developers.openai.com/api/docs/guides/structured-outputs
  (the ChatGPT/Codex OAuth backend takes the same `text.format`, see `create_text_param_for_request` in
  https://github.com/openai/codex/blob/main/codex-rs/codex-api/src/common.rs)
* Azure OpenAI (same shapes, API version 2024-08-01-preview or later):
  https://learn.microsoft.com/en-us/azure/ai-foundry/openai/how-to/structured-outputs
* Anthropic Messages `output_config: {format: {type: "json_schema", schema}}` (GA, no beta header;
  also on Bedrock InvokeModel and Vertex): https://platform.claude.com/docs/en/build-with-claude/structured-outputs
* Anthropic forced tool `tool_choice: {type: "tool", name}`, used only for `ClaudeModel.custom` ids:
  https://platform.claude.com/docs/en/agents-and-tools/tool-use/implement-tool-use
  (Opus 5.5, Sonnet 5.5 and Fable 5.1 reject forced tool use, so known models use `output_config`.)
* Bedrock Converse `outputConfig: {textFormat: {type: "json_schema", structure: {jsonSchema: {schema: <string>, name, description?}}}}`:
  https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html
  https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_OutputFormat.html
  https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_JsonSchemaDefinition.html
  and `toolConfig.toolChoice: {tool: {name}}`:
  https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolChoice.html
* Gemini `generationConfig: {responseMimeType: "application/json", responseJsonSchema}`:
  https://ai.google.dev/api/generate-content (GenerationConfig);
  Code Assist (`v1internal`) forwards the same `generationConfig` inside `request`, see
  https://github.com/google-gemini/gemini-cli/blob/main/packages/core/src/code_assist/converter.ts
* OpenRouter `response_format` json_schema (fails on endpoints without support):
  https://openrouter.ai/docs/guides/features/structured-outputs
* xAI Grok json_schema: https://docs.x.ai/docs/guides/structured-outputs
* Mistral json_schema `{name, description?, schema, strict}`: https://docs.mistral.ai/openapi.yaml (`JsonSchema`)
* Kimi/Moonshot json_schema (`name`, `schema`, `strict`): https://platform.kimi.ai/docs/api/chat
* Qwen (DashScope compatible mode) json_schema only for the Qwen3.7-Plus/Flash/Max and
  Qwen3.8-Max/Flash series, `json_object` otherwise:
  https://www.alibabacloud.com/help/en/model-studio/qwen-structured-output
* DeepSeek `json_object` only (prompt must mention "json" and give an example):
  https://api-docs.deepseek.com/guides/json_mode
* Z.AI GLM `json_object` only: https://docs.z.ai/api-reference/llm/chat-completion
* MiniMax OpenAI-compatible API does not document `response_format`:
  https://platform.minimax.io/docs/api-reference/text-openai-api
-/

namespace Leanllm

open Lean

/-- Ask the model for one JSON value following `schema`. -/
structure ResponseFormat where
  /-- Format (or forced tool) name. OpenAI requires `[a-zA-Z0-9_-]{1,64}`. -/
  name        : String         := "result"
  /-- JSON Schema for the value. -/
  schema      : Json
  /-- Optional description, sent where the provider accepts one. -/
  description : Option String  := none
  /-- Ask for strict schema adherence where the provider supports it (OpenAI, Kimi, Qwen, Mistral, xAI). -/
  strict      : Bool           := true
  deriving Repr, BEq, Inhabited

/-- How a request asks for JSON output. -/
inductive StructuredStrategy where
  /-- The provider's native JSON Schema mode. -/
  | nativeSchema
  /-- A single tool whose input schema is the output schema, forced with `tool_choice`. -/
  | forcedTool
  /-- JSON mode (`response_format: {type: "json_object"}`) plus a prompt describing the schema. -/
  | jsonObject
  /-- A prompt describing the schema, then JSON extraction from the reply text. -/
  | promptOnly
  deriving Repr, BEq, Inhabited

namespace StructuredStrategy

/-- Whether `completionAs` should add a schema instruction to the prompt. -/
def needsPrompt : StructuredStrategy → Bool
  | .jsonObject | .promptOnly => true
  | .nativeSchema | .forcedTool => false

end StructuredStrategy

namespace ResponseFormat

/-- Qwen models that DashScope documents as supporting `json_schema`. -/
private def qwenSupportsJsonSchema : QwenModel → Bool
  | .qwen3_8Max | .qwen3_8Flash | .qwen3_7Max | .qwen3_7Plus => true
  | _ => false

/-- Chat Completions strategy per provider and model. Unlisted hosts get `promptOnly`. -/
private def openaiChatStrategy (provider : Provider) (model : Model) : StructuredStrategy :=
  match provider, model with
  | .direct, .openai _       => .nativeSchema
  | .azure _, _              => .nativeSchema
  | .openRouter _, _         => .nativeSchema
  | .direct, .grok _         => .nativeSchema
  | .direct, .mistral _      => .nativeSchema
  | .direct, .kimi _         => .nativeSchema
  | .direct, .qwen m         => if qwenSupportsJsonSchema m then .nativeSchema else .jsonObject
  | .direct, .deepseek _     => .jsonObject
  | .direct, .glm _          => .jsonObject
  | _, _                     => .promptOnly

/-- Pick how to ask for structured output for an effective wire API, provider, and model. -/
def strategyFor (api : WireApi) (provider : Provider) (model : Model) : StructuredStrategy :=
  match api with
  | .anthropicMessages | .bedrockInvoke | .bedrockConverse =>
    match model with
    | .claude (.custom _) => .forcedTool
    | .claude _           => .nativeSchema
    | _                   => .promptOnly
  | .openaiResponses => .nativeSchema
  | .geminiGenerate  => .nativeSchema
  | .openaiChat | .auto => openaiChatStrategy provider model

/-- Instruction text added as a system message when the strategy needs a prompt. -/
def instruction (rf : ResponseFormat) : String :=
  let desc := match rf.description with
    | some d => s!" {d}"
    | none   => ""
  s!"Respond with a single JSON object named \"{rf.name}\" that conforms to this JSON Schema.{desc} " ++
  s!"Output only the JSON, with no prose and no code fences.\nJSON Schema: {rf.schema.compress}"

private def optStr (key : String) : Option String → List (String × Json)
  | some v => [(key, toJson v)]
  | none   => []

/-- Read an object's fields (empty for non-objects). -/
private def objFields (j : Json) : List (String × Json) :=
  match j with
  | .obj kvs => kvs.toList
  | _        => []

/-- Shallow-merge `extra` fields into the object stored at `key` in `j` (creating it if absent). -/
private def mergeObjField (j : Json) (key : String) (extra : List (String × Json)) : Json :=
  let existing := (j.getObjVal? key).toOption.map objFields |>.getD []
  let kept := existing.filter fun (k, _) => !(extra.any fun (k', _) => k' == k)
  j.setObjVal! key (Json.mkObj (kept ++ extra))

/-- Append `item` to the array stored at `key` in `j` (creating it if absent). -/
private def appendArrField (j : Json) (key : String) (item : Json) : Json :=
  let existing := ((j.getObjValAs? (Array Json) key).toOption).getD #[]
  j.setObjVal! key (toJson (existing.push item))

private def openaiJsonSchemaObj (rf : ResponseFormat) : Json :=
  Json.mkObj <|
    [("name", toJson rf.name)] ++
    optStr "description" rf.description ++
    [("schema", rf.schema),
     ("strict", toJson rf.strict)]

private def applyOpenAIChat (rf : ResponseFormat) (strategy : StructuredStrategy) (payload : Json) : Json :=
  match strategy with
  | .nativeSchema =>
    payload.setObjVal! "response_format" (Json.mkObj [
      ("type", "json_schema"),
      ("json_schema", openaiJsonSchemaObj rf)
    ])
  | .jsonObject =>
    payload.setObjVal! "response_format" (Json.mkObj [("type", "json_object")])
  | .forcedTool | .promptOnly => payload

private def applyOpenAIResponses (rf : ResponseFormat) (payload : Json) : Json :=
  mergeObjField payload "text" [("format", Json.mkObj <|
    [("type", toJson "json_schema"),
     ("name", toJson rf.name)] ++
    optStr "description" rf.description ++
    [("schema", rf.schema),
     ("strict", toJson rf.strict)])]

private def applyAnthropic (rf : ResponseFormat) (strategy : StructuredStrategy)
    (isClaudeOAuth : Bool) (payload : Json) : Json :=
  match strategy with
  | .nativeSchema =>
    mergeObjField payload "output_config" [("format", Json.mkObj [
      ("type", "json_schema"),
      ("schema", rf.schema)
    ])]
  | .forcedTool =>
    let wireName := if isClaudeOAuth then Auth.Claude.mapToolNameForOAuth rf.name else rf.name
    let tool := Json.mkObj [
      ("name", toJson wireName),
      ("description", toJson (rf.description.getD "Return the final answer as structured data.")),
      ("input_schema", rf.schema)
    ]
    (appendArrField payload "tools" tool).setObjVal! "tool_choice"
      (Json.mkObj [("type", "tool"), ("name", toJson wireName)])
  | .jsonObject | .promptOnly => payload

private def applyBedrockConverse (rf : ResponseFormat) (strategy : StructuredStrategy) (payload : Json) : Json :=
  match strategy with
  | .nativeSchema =>
    -- `JsonSchemaDefinition.schema` is a string holding the schema JSON.
    mergeObjField payload "outputConfig" [("textFormat", Json.mkObj [
      ("type", "json_schema"),
      ("structure", Json.mkObj [("jsonSchema", Json.mkObj <|
        [("schema", toJson rf.schema.compress),
         ("name", toJson rf.name)] ++
        optStr "description" rf.description)])
    ])]
  | .forcedTool =>
    let toolSpec := Json.mkObj [("toolSpec", Json.mkObj [
      ("name", toJson rf.name),
      ("description", toJson (rf.description.getD "Return the final answer as structured data.")),
      ("inputSchema", Json.mkObj [("json", rf.schema)])
    ])]
    let toolCfg := (payload.getObjVal? "toolConfig").toOption.getD (Json.mkObj [])
    let toolCfg := (appendArrField toolCfg "tools" toolSpec).setObjVal! "toolChoice"
      (Json.mkObj [("tool", Json.mkObj [("name", toJson rf.name)])])
    payload.setObjVal! "toolConfig" toolCfg
  | .jsonObject | .promptOnly => payload

private def applyGemini (rf : ResponseFormat) (codeAssist : Bool) (payload : Json) : Json :=
  let extra : List (String × Json) := [
    ("responseMimeType", "application/json"),
    ("responseJsonSchema", rf.schema)
  ]
  if codeAssist then
    match (payload.getObjVal? "request").toOption with
    | some inner => payload.setObjVal! "request" (mergeObjField inner "generationConfig" extra)
    | none       => payload
  else
    mergeObjField payload "generationConfig" extra

/-- Add the structured-output fields for `api` to an encoded request body.
    `isClaudeOAuth` maps the forced tool name like other Claude OAuth tools;
    `codeAssist` puts Gemini fields under the Code Assist `request` envelope. -/
def applyToPayload (rf : ResponseFormat) (api : WireApi) (provider : Provider) (model : Model)
    (isClaudeOAuth : Bool := false) (codeAssist : Bool := false) (payload : Json) : Json :=
  let strategy := strategyFor api provider model
  match api with
  | .anthropicMessages | .bedrockInvoke => applyAnthropic rf strategy isClaudeOAuth payload
  | .bedrockConverse                    => applyBedrockConverse rf strategy payload
  | .openaiResponses                    => applyOpenAIResponses rf payload
  | .geminiGenerate                     => applyGemini rf codeAssist payload
  | .openaiChat | .auto                 => applyOpenAIChat rf strategy payload

end ResponseFormat
end Leanllm
