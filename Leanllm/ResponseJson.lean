import Lean.Data.Json
import Leanllm.Provider
import Leanllm.Message
import Leanllm.Response

/-!
# JSON codecs for `LLMResponse` and its parts

A stable, provider-independent JSON form of `LLMResponse`, `Message`, `ContentPart`, `StopReason`,
`Usage`, `Cost` and `Model`, used by `Leanllm.Replay` to store recorded responses on disk.

Decoders ignore unknown fields, so records written by a newer leanllm still load, and optional
fields missing from older records (`cost`, `cacheWriteLongTtlTokens`) decode as `none`.
-/

namespace Leanllm

open Lean

deriving instance ToJson, FromJson for ClaudeModel
deriving instance ToJson, FromJson for OpenAIModel
deriving instance ToJson, FromJson for GeminiModel
deriving instance ToJson, FromJson for DeepSeekModel
deriving instance ToJson, FromJson for KimiModel
deriving instance ToJson, FromJson for GlmModel
deriving instance ToJson, FromJson for QwenModel
deriving instance ToJson, FromJson for MiniMaxModel
deriving instance ToJson, FromJson for GrokModel
deriving instance ToJson, FromJson for MistralModel

namespace ResponseJson

private def optKV [ToJson α] (key : String) : Option α → List (String × Json)
  | some v => [(key, toJson v)]
  | none   => []

private def optVal? [FromJson α] (j : Json) (key : String) : Except String (Option α) :=
  match j.getObjVal? key with
  | .error _   => .ok none
  | .ok .null  => .ok none
  | .ok v      => some <$> fromJson? v

private def familyOf : Model → String × Json
  | .claude m   => ("claude", toJson m)
  | .openai m   => ("openai", toJson m)
  | .gemini m   => ("gemini", toJson m)
  | .deepseek m => ("deepseek", toJson m)
  | .kimi m     => ("kimi", toJson m)
  | .glm m      => ("glm", toJson m)
  | .qwen m     => ("qwen", toJson m)
  | .minimax m  => ("minimax", toJson m)
  | .grok m     => ("grok", toJson m)
  | .mistral m  => ("mistral", toJson m)

/-- Decode the family's variant; if the variant is unknown (e.g. renamed), fall back to `.custom id`. -/
private def variantOr [FromJson α] (variant : Json) (custom : String → α) (id : String) : α :=
  match fromJson? variant with
  | .ok m    => m
  | .error _ => custom id

end ResponseJson

open ResponseJson

/-- `{"family": "claude", "id": "claude-opus-5-5", "variant": <constructor>}`. -/
instance : ToJson Model where
  toJson m :=
    let (family, variant) := familyOf m
    Json.mkObj [("family", family), ("id", m.toModelId), ("variant", variant)]

instance : FromJson Model where
  fromJson? j := do
    let family ← j.getObjValAs? String "family"
    let id ← j.getObjValAs? String "id"
    let variant := (j.getObjVal? "variant").toOption.getD Json.null
    match family with
    | "claude"   => pure (.claude   (variantOr variant .custom id))
    | "openai"   => pure (.openai   (variantOr variant .custom id))
    | "gemini"   => pure (.gemini   (variantOr variant .custom id))
    | "deepseek" => pure (.deepseek (variantOr variant .custom id))
    | "kimi"     => pure (.kimi     (variantOr variant .custom id))
    | "glm"      => pure (.glm      (variantOr variant .custom id))
    | "qwen"     => pure (.qwen     (variantOr variant .custom id))
    | "minimax"  => pure (.minimax  (variantOr variant .custom id))
    | "grok"     => pure (.grok     (variantOr variant .custom id))
    | "mistral"  => pure (.mistral  (variantOr variant .custom id))
    | other      => throw s!"unknown model family '{other}'"

instance : ToJson Role where
  toJson
    | .system    => "system"
    | .developer => "developer"
    | .user      => "user"
    | .assistant => "assistant"
    | .tool      => "tool"

instance : FromJson Role where
  fromJson? j := do
    match ← j.getStr? with
    | "system"    => pure .system
    | "developer" => pure .developer
    | "user"      => pure .user
    | "assistant" => pure .assistant
    | "tool"      => pure .tool
    | other       => throw s!"unknown role '{other}'"

instance : ToJson ImageSource where
  toJson
    | .url url              => Json.mkObj [("type", "url"), ("url", url)]
    | .base64 mediaType data => Json.mkObj [("type", "base64"), ("mediaType", mediaType), ("data", data)]

instance : FromJson ImageSource where
  fromJson? j := do
    match ← j.getObjValAs? String "type" with
    | "url"    => return .url (← j.getObjValAs? String "url")
    | "base64" => return .base64 (← j.getObjValAs? String "mediaType") (← j.getObjValAs? String "data")
    | other    => throw s!"unknown image source type '{other}'"

instance : ToJson ToolCall where
  toJson c := Json.mkObj [("id", c.id), ("name", c.name), ("arguments", c.arguments)]

instance : FromJson ToolCall where
  fromJson? j := do
    return { id := ← j.getObjValAs? String "id", name := ← j.getObjValAs? String "name"
             arguments := (j.getObjVal? "arguments").toOption.getD Json.null }

instance : ToJson ToolResult where
  toJson r := Json.mkObj [("toolCallId", r.toolCallId), ("content", r.content), ("isError", r.isError)]

instance : FromJson ToolResult where
  fromJson? j := do
    return { toolCallId := ← j.getObjValAs? String "toolCallId"
             content := ← j.getObjValAs? String "content"
             isError := ((← optVal? j "isError") : Option Bool).getD false }

instance : ToJson ContentPart where
  toJson
    | .text t           => Json.mkObj [("type", "text"), ("text", t)]
    | .image src        => Json.mkObj [("type", "image"), ("source", toJson src)]
    | .toolCall c       => Json.mkObj [("type", "toolCall"), ("call", toJson c)]
    | .toolResult r     => Json.mkObj [("type", "toolResult"), ("result", toJson r)]
    | .thinking t sig   => Json.mkObj ([("type", Json.str "thinking"), ("thought", toJson t)] ++ optKV "signature" sig)

instance : FromJson ContentPart where
  fromJson? j := do
    match ← j.getObjValAs? String "type" with
    | "text"       => return .text (← j.getObjValAs? String "text")
    | "image"      => return .image (← j.getObjValAs? ImageSource "source")
    | "toolCall"   => return .toolCall (← j.getObjValAs? ToolCall "call")
    | "toolResult" => return .toolResult (← j.getObjValAs? ToolResult "result")
    | "thinking"   => return .thinking (← j.getObjValAs? String "thought") (← optVal? j "signature")
    | other        => throw s!"unknown content part type '{other}'"

instance : ToJson Message where
  toJson m := Json.mkObj ([("role", toJson m.role), ("content", toJson m.content)] ++ optKV "name" m.name)

instance : FromJson Message where
  fromJson? j := do
    return { role := ← j.getObjValAs? Role "role"
             content := ← j.getObjValAs? (List ContentPart) "content"
             name := ← optVal? j "name" }

instance : ToJson StopReason where
  toJson
    | .endTurn          => Json.mkObj [("type", "endTurn")]
    | .maxTokens        => Json.mkObj [("type", "maxTokens")]
    | .stopSequence seq => Json.mkObj ([("type", Json.str "stopSequence")] ++ optKV "sequence" seq)
    | .toolUse          => Json.mkObj [("type", "toolUse")]
    | .contentFilter    => Json.mkObj [("type", "contentFilter")]
    | .other raw        => Json.mkObj [("type", "other"), ("raw", raw)]

instance : FromJson StopReason where
  fromJson? j := do
    match ← j.getObjValAs? String "type" with
    | "endTurn"       => pure .endTurn
    | "maxTokens"     => pure .maxTokens
    | "stopSequence"  => return .stopSequence (← optVal? j "sequence")
    | "toolUse"       => pure .toolUse
    | "contentFilter" => pure .contentFilter
    | "other"         => return .other (← j.getObjValAs? String "raw")
    | other           => throw s!"unknown stop reason '{other}'"

instance : ToJson Usage where
  toJson u := Json.mkObj ([("inputTokens", toJson u.inputTokens), ("outputTokens", toJson u.outputTokens)]
    ++ optKV "cacheReadTokens" u.cacheReadTokens
    ++ optKV "cacheWriteTokens" u.cacheWriteTokens
    ++ optKV "reasoningTokens" u.reasoningTokens
    ++ optKV "cacheWriteLongTtlTokens" u.cacheWriteLongTtlTokens)

instance : FromJson Usage where
  fromJson? j := do
    return { inputTokens := ← j.getObjValAs? Nat "inputTokens"
             outputTokens := ← j.getObjValAs? Nat "outputTokens"
             cacheReadTokens := ← optVal? j "cacheReadTokens"
             cacheWriteTokens := ← optVal? j "cacheWriteTokens"
             reasoningTokens := ← optVal? j "reasoningTokens"
             cacheWriteLongTtlTokens := ← optVal? j "cacheWriteLongTtlTokens" }

instance : ToJson CostBasis where
  toJson
    | .apiKey       => "apiKey"
    | .subscription => "subscription"

instance : FromJson CostBasis where
  fromJson? j := do
    match ← j.getStr? with
    | "apiKey"       => pure .apiKey
    | "subscription" => pure .subscription
    | other          => throw s!"unknown cost basis '{other}'"

instance : ToJson Cost where
  toJson c := Json.mkObj [("nanoUsd", toJson c.nanoUsd), ("basis", toJson c.basis),
    ("rateVersion", toJson c.rateVersion)]

instance : FromJson Cost where
  fromJson? j := do
    return { nanoUsd := ← j.getObjValAs? Nat "nanoUsd"
             basis := ← j.getObjValAs? CostBasis "basis"
             rateVersion := ← j.getObjValAs? String "rateVersion" }

instance : ToJson LLMResponse where
  toJson r := Json.mkObj ([("id", toJson r.id), ("model", toJson r.model), ("message", toJson r.message),
    ("stopReason", toJson r.stopReason)] ++ optKV "usage" r.usage ++ optKV "cost" r.cost)

instance : FromJson LLMResponse where
  fromJson? j := do
    return { id := ← j.getObjValAs? String "id"
             model := ← j.getObjValAs? Model "model"
             message := ← j.getObjValAs? Message "message"
             stopReason := ← j.getObjValAs? StopReason "stopReason"
             usage := ← optVal? j "usage"
             cost := ← optVal? j "cost" }

end Leanllm
