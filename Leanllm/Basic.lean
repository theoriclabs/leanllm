import LeanHttp
import Leanllm.Provider
import Leanllm.Models
import Leanllm.Message
import Leanllm.Error
import Leanllm.Response
import Leanllm.Codec
import Leanllm.Transport

namespace Leanllm

/-- Runtime context for `LLMM`: carries `ModelConfig` and an optional reusable `LeanHttp.Session`. -/
structure LLMContext where
  config  : ModelConfig
  session : Option LeanHttp.Session := none

/-- The LLM effect monad: reads `LLMContext`, fails with `LLMError`, performs `IO`. -/
abbrev LLMM (α : Type) := ReaderT LLMContext (ExceptT LLMError IO) α

/-- Execute a single LLM call using `LeanHttp` (with an optional reusable `LeanHttp.Session`). -/
def callWithSession (cfg : ModelConfig) (messages : List Message)
    (session : Option LeanHttp.Session := none) : ExceptT LLMError IO LLMResponse := do
  if !cfg.provider.supportsModel cfg.model then
    throw (.invalidRequest
      (.unsupportedFeature s!"Provider {repr cfg.provider} does not support model {cfg.model}")
      "Unsupported model for provider")
  let apiKey ← Transport.resolveApiKey cfg
  let payload ← match Codec.encodeRequest cfg messages with
    | .ok j      => pure j
    | .error err => throw err
  let req ← match Transport.buildHttpRequest cfg apiKey payload with
    | .ok r      => pure r
    | .error err => throw err
  let respJson ← Transport.sendJson req session
  match Codec.decodeResponse cfg respJson with
  | .ok resp   => return resp
  | .error err => throw err

/-- Execute a single one-shot LLM call for a given `ModelConfig` and conversation history. -/
def call (cfg : ModelConfig) (messages : List Message) : ExceptT LLMError IO LLMResponse :=
  callWithSession cfg messages none

/-- Stream an LLM completion over SSE using `LeanHttp.Sse`, invoking `onDelta` for each text chunk
    and returning the concatenated response text. -/
def callStreamWithSession (cfg : ModelConfig) (messages : List Message)
    (onDelta : String → IO Unit)
    (session : Option LeanHttp.Session := none) : ExceptT LLMError IO String := do
  if !cfg.provider.supportsModel cfg.model then
    throw (.invalidRequest
      (.unsupportedFeature s!"Provider {repr cfg.provider} does not support model {cfg.model}")
      "Unsupported model for provider")
  let apiKey ← Transport.resolveApiKey cfg
  let payload ← match Codec.encodeStreamRequest cfg messages with
    | .ok j      => pure j
    | .error err => throw err
  let req ← match Transport.buildStreamHttpRequest cfg apiKey payload with
    | .ok r      => pure r
    | .error err => throw err
  let accRef ← liftM (m := IO) (IO.mkRef "")
  Transport.sendSse req session fun ev => do
    if let some delta := Codec.decodeSseTextDelta cfg ev.event ev.data then
      accRef.modify (· ++ delta)
      onDelta delta
  let fullText ← liftM (m := IO) accRef.get
  return fullText

/-- Stream a one-shot LLM call over SSE, invoking `onDelta` for each text chunk. -/
def callStream (cfg : ModelConfig) (messages : List Message)
    (onDelta : String → IO Unit) : ExceptT LLMError IO String :=
  callStreamWithSession cfg messages onDelta none

namespace LLMM

/-- Read the current `ModelConfig` inside `LLMM`. -/
def getConfig : LLMM ModelConfig := do
  return (← read).config

/-- Modify the `ModelConfig` for a sub-computation. -/
def withConfig (f : ModelConfig → ModelConfig) (act : LLMM α) : LLMM α :=
  withReader (fun ctx => { ctx with config := f ctx.config }) act

/-- Run an `LLMM` computation with a fresh `LeanHttp.Session` that is automatically closed on exit. -/
def run (cfg : ModelConfig) (act : LLMM α) : IO (Except LLMError α) := do
  match ← LeanHttp.Session.new with
  | .error err =>
    return .error (Transport.fromHttpError err)
  | .ok session =>
    try
      let res ← ExceptT.run (ReaderT.run act { config := cfg, session := some session })
      session.close
      return res
    catch e =>
      session.close
      throw e

/-- Run an `LLMM` computation using an existing `LeanHttp.Session`. -/
def runWithSession (cfg : ModelConfig) (session : LeanHttp.Session) (act : LLMM α) : IO (Except LLMError α) :=
  ExceptT.run (ReaderT.run act { config := cfg, session := some session })

/-- Throw a typed `LLMError` inside `LLMM`. -/
def throw (err : LLMError) : LLMM α :=
  throwThe LLMError err

/-- Swap the `Provider` (e.g. `.direct`, `.openRouter`, `.bedrock`, `.azure`) for a sub-computation. -/
def withProvider (provider : Provider) (act : LLMM α) : LLMM α :=
  withConfig (fun cfg => { cfg with provider }) act

/-- Swap the `WireApi` (e.g. `.openaiChat`, `.openaiResponses`, `.bedrockConverse`) for a sub-computation. -/
def withWireApi (wireApi : WireApi) (act : LLMM α) : LLMM α :=
  withConfig (fun cfg => { cfg with wireApi }) act

/-- Swap the `Model` for a sub-computation. -/
def withModel (model : Model) (act : LLMM α) : LLMM α :=
  withConfig (fun cfg => { cfg with model }) act

end LLMM

/-- Monadic LLM completion reading `ModelConfig` and `LeanHttp.Session` from `LLMM` context. -/
def completion (messages : List Message) (tools : List ToolSpec := []) : LLMM LLMResponse := do
  let ctx ← read
  let cfg := if tools.isEmpty then ctx.config else { ctx.config with tools := ctx.config.tools ++ tools }
  callWithSession cfg messages ctx.session

/-- Convenience helper for a single user prompt returning the assistant's text in `LLMM`. -/
def ask (prompt : String) : LLMM String := do
  let resp ← completion [Message.user prompt]
  return resp.text

/-- Monadic streaming LLM completion over SSE in `LLMM`. -/
def completionStream (messages : List Message) (onDelta : String → IO Unit) : LLMM String := do
  let ctx ← read
  callStreamWithSession ctx.config messages onDelta ctx.session

/-- Stream a single user prompt over SSE in `LLMM`. -/
def askStream (prompt : String) (onDelta : String → IO Unit) : LLMM String :=
  completionStream [Message.user prompt] onDelta

end Leanllm
