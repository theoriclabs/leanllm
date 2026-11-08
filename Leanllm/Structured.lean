import Lean.Data.Json
import Leanllm.Basic
import Leanllm.ResponseFormat

/-!
# Typed structured output

`completionAs` asks the model for a JSON value following a JSON Schema and decodes it into `β`
with `FromJson`. Each wire API uses the provider's native structured-output mode where its docs
describe one (see `Leanllm.ResponseFormat`). Elsewhere the schema goes into the prompt and the
first JSON object in the reply text is decoded.

A reply that cannot be decoded gives `StructuredError.schemaViolation` with the raw reply text.
-/

namespace Leanllm

open Lean

/-- Errors from `completionAs`. Separate from `LLMError` so existing exhaustive matches still hold. -/
inductive StructuredError where
  /-- The underlying LLM call failed. -/
  | llm (e : LLMError)
  /-- The reply did not decode to the requested type.
      `expected` names the format and its schema, `raw` is the reply text (or forced-tool input),
      and `decodeError` says what went wrong. -/
  | schemaViolation (expected : String) (raw : String) (decodeError : String)
  deriving Repr, BEq, Inhabited

namespace Structured

/-- Contents of the first fenced code block (```` ```json ... ``` ````), without its language tag. -/
def fencedBlock? (s : String) : Option String :=
  match s.splitOn "```" with
  | _ :: inner :: _ :: _ =>
    match inner.splitOn "\n" with
    | tag :: rest@(_ :: _) =>
      let tag := tag.trimAscii.toString
      -- A first line made only of letters is a language tag (`json`, `JSON`, ...).
      if tag.all Char.isAlpha then some (String.intercalate "\n" rest) else some inner
    | _ => some inner
  | _ => none

/-- The balanced `{ ... }` starting at index `start` of `cs` (string-literal aware), if it closes. -/
private def balancedFrom (cs : Array Char) (start : Nat) : Option String := Id.run do
  let mut depth : Nat := 0
  let mut inStr := false
  let mut esc := false
  let mut acc := ""
  for i in [start:cs.size] do
    let c := cs[i]!
    acc := acc.push c
    if inStr then
      if esc then esc := false
      else if c == '\\' then esc := true
      else if c == '"' then inStr := false
    else if c == '"' then inStr := true
    else if c == '{' then depth := depth + 1
    else if c == '}' then
      depth := depth - 1
      if depth == 0 then return some acc
  return none

/-- The first top-level JSON object in `s` that parses, skipping brace runs that are not JSON. -/
def firstJsonObject? (s : String) : Option Json := Id.run do
  let cs := s.toList.toArray
  for i in [0:cs.size] do
    if cs[i]! == '{' then
      if let some candidate := balancedFrom cs i then
        if let .ok j := Json.parse candidate then
          return some j
  return none

/-- Find the JSON value in a reply: the whole text, then a fenced block, then the first JSON object. -/
def extractJson? (text : String) : Option Json :=
  let trimmed := text.trimAscii.toString
  match Json.parse trimmed with
  | .ok j => some j
  | .error _ =>
    let fromFence := (fencedBlock? trimmed).bind fun inner =>
      match Json.parse inner.trimAscii.toString with
      | .ok j    => some j
      | .error _ => firstJsonObject? inner
    fromFence.or (firstJsonObject? trimmed)

/-- The `expected` text carried by `schemaViolation`. -/
def expectedOf (rf : ResponseFormat) : String :=
  s!"{rf.name}: {rf.schema.compress}"

/-- Decode a response into `β`: a forced tool call named `rf.name` if present, else the reply text. -/
def decodeResponse [FromJson β] (rf : ResponseFormat) (resp : LLMResponse) : Except StructuredError β := do
  let viol := StructuredError.schemaViolation (expectedOf rf)
  let (raw, json?) := match resp.toolCalls.find? (·.name == rf.name) with
    | some tc => (tc.arguments.compress, some tc.arguments)
    | none    => (resp.text, extractJson? resp.text)
  let some json := json?
    | throw (viol raw "no JSON value found in the reply")
  match fromJson? json with
  | .ok v      => return v
  | .error err => throw (viol raw err)

/-- Add the schema instruction to the system prompt (merged into a leading system message if any). -/
def withInstruction (rf : ResponseFormat) (messages : List Message) : List Message :=
  let instr := rf.instruction
  match messages with
  | m :: rest =>
    if m.role == .system then { m with content := m.content ++ [.text instr] } :: rest
    else Message.system instr :: messages
  | [] => [Message.system instr]

/-- The config and messages `completionAs` sends for `rf`. -/
def prepare (cfg : ModelConfig) (messages : List Message) (rf : ResponseFormat) :
    ModelConfig × List Message :=
  let strategy := ResponseFormat.strategyFor cfg.effectiveWireApi cfg.provider cfg.model
  let messages := if strategy.needsPrompt then withInstruction rf messages else messages
  ({ cfg with responseFormat := some rf }, messages)

end Structured

/-- `completionAs` with the transport passed in (`call`, a session-bound call, or a test double). -/
def completionAsVia [FromJson β]
    (callFn : ModelConfig → List Message → ExceptT LLMError IO LLMResponse)
    (cfg : ModelConfig) (messages : List Message) (schema : Json)
    (name : String := "result") (strict : Bool := true) (description : Option String := none) :
    ExceptT StructuredError IO (β × LLMResponse) := do
  let rf : ResponseFormat := { name, schema, strict, description }
  let (cfg', messages') := Structured.prepare cfg messages rf
  let resp ← ExceptT.adapt StructuredError.llm (callFn cfg' messages')
  match Structured.decodeResponse (β := β) rf resp with
  | .ok v      => return (v, resp)
  | .error err => throw err

/-- Ask for a JSON value following `schema` and decode it into `β`.
    Returns the value and the full `LLMResponse` (usage, stop reason). -/
def completionAs [FromJson β] (cfg : ModelConfig) (messages : List Message) (schema : Json)
    (name : String := "result") (strict : Bool := true) (description : Option String := none) :
    ExceptT StructuredError IO (β × LLMResponse) :=
  completionAsVia call cfg messages schema name strict description

/-- `completionAs` over an existing `LeanHttp.Session`. -/
def completionAsWithSession [FromJson β] (cfg : ModelConfig) (messages : List Message) (schema : Json)
    (session : Option LeanHttp.Session) (name : String := "result") (strict : Bool := true)
    (description : Option String := none) : ExceptT StructuredError IO (β × LLMResponse) :=
  completionAsVia (fun c m => callWithSession c m session) cfg messages schema name strict description

/-- `completionAs` inside `LLMM`, using its config and session. Every failure, including LLM errors,
    comes back as a `StructuredError` value. -/
def LLMM.completionAs [FromJson β] (messages : List Message) (schema : Json)
    (name : String := "result") (strict : Bool := true) (description : Option String := none) :
    LLMM (Except StructuredError (β × LLMResponse)) := do
  let ctx ← read
  (completionAsWithSession ctx.config messages schema ctx.session name strict description).run

end Leanllm
