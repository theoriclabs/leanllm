import Leanllm.Provider
import Leanllm.Message
import Leanllm.Auth

namespace Leanllm

structure ModelConfig where
  model       : Model
  provider    : Provider      := .direct
  wireApi     : WireApi       := .auto
  auth        : AuthMode      := .auto
  temperature : Option Float  := none
  maxTokens   : Option Nat    := none
  topP        : Option Float  := none
  stop        : List String   := []
  tools       : List ToolSpec := []
  apiKey      : Option String := none
  baseUrl     : Option String := none
  deriving Repr, BEq, Inhabited

namespace ModelConfig

/-- Effective `WireApi` after resolving `.auto` against `provider`, `model`, and `auth`. -/
def effectiveWireApi (cfg : ModelConfig) : WireApi :=
  match cfg.wireApi with
  | .auto =>
    match cfg.provider, cfg.model, cfg.auth with
    | .direct, .openai _, .oauth _ => .openaiResponses
    | _, _, _ => cfg.provider.defaultWireApi cfg.model
  | api => api

/-- Effective `WireApi` taking into account the resolved runtime credential
    (e.g. when `AuthMode.auto` resolves to `ResolvedAuth.openaiOAuth`, which requires `.openaiResponses`). -/
def effectiveWireApiForAuth (cfg : ModelConfig) (resolved : ResolvedAuth) : WireApi :=
  match cfg.wireApi, resolved with
  | .auto, .openaiOAuth _ => .openaiResponses
  | _, _                  => cfg.effectiveWireApi

/-- Effective model identifier string for the configured `provider`. -/
def effectiveModelId (cfg : ModelConfig) : String :=
  cfg.provider.resolveModelId cfg.model

end ModelConfig
end Leanllm
