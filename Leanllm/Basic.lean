import Leanllm.Provider
import Leanllm.Models
import Leanllm.Message
import Leanllm.Error
import Leanllm.Response

namespace Leanllm

/-- The LLM effect monad: reads `ModelConfig`, fails with `LLMError`, performs `IO`. -/
abbrev LLMM (α : Type) := ReaderT ModelConfig (ExceptT LLMError IO) α

namespace LLMM

/-- Run an `LLMM` computation with a given `ModelConfig`. -/
def run (cfg : ModelConfig) (act : LLMM α) : IO (Except LLMError α) :=
  ExceptT.run (ReaderT.run act cfg)

/-- Throw a typed `LLMError` inside `LLMM`. -/
def throw (err : LLMError) : LLMM α :=
  throwThe LLMError err

/-- Swap the `Provider` (e.g. `.direct`, `.openRouter`, `.bedrock`, `.azure`) for a sub-computation. -/
def withProvider (provider : Provider) (act : LLMM α) : LLMM α :=
  withReader (fun cfg => { cfg with provider }) act

/-- Swap the `WireApi` (e.g. `.openaiChat`, `.openaiResponses`, `.bedrockConverse`) for a sub-computation. -/
def withWireApi (wireApi : WireApi) (act : LLMM α) : LLMM α :=
  withReader (fun cfg => { cfg with wireApi }) act

/-- Swap the `Model` for a sub-computation. -/
def withModel (model : Model) (act : LLMM α) : LLMM α :=
  withReader (fun cfg => { cfg with model }) act

end LLMM

/-- Execute a single LLM call for a given `ModelConfig` and conversation history. -/
def call (cfg : ModelConfig) (_messages : List Message) : ExceptT LLMError IO LLMResponse := do
  if !cfg.provider.supportsModel cfg.model then
    throw (.invalidRequest
      (.unsupportedFeature s!"Provider {repr cfg.provider} does not support model {cfg.model}")
      "Unsupported model for provider")
  let api := cfg.effectiveWireApi
  let modelId := cfg.effectiveModelId
  throw (.provider
    (.statusCode 501 s!"Backend not implemented for model={modelId} wireApi={repr api} provider={repr cfg.provider}")
    "Not implemented")

/-- Monadic LLM completion reading `ModelConfig` from context. -/
def completion (messages : List Message) (tools : List ToolSpec := []) : LLMM LLMResponse := do
  let cfg ← read
  let cfg := if tools.isEmpty then cfg else { cfg with tools := cfg.tools ++ tools }
  call cfg messages

end Leanllm
