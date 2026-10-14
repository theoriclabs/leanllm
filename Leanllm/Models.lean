import Leanllm.Provider
import Leanllm.Message

namespace Leanllm

structure ModelConfig where
  model       : Model
  provider    : Provider      := .direct
  wireApi     : WireApi       := .auto
  temperature : Option Float  := none
  maxTokens   : Option Nat    := none
  topP        : Option Float  := none
  stop        : List String   := []
  tools       : List ToolSpec := []
  apiKey      : Option String := none
  baseUrl     : Option String := none
  deriving Repr, BEq, Inhabited

namespace ModelConfig

/-- Effective `WireApi` after resolving `.auto` against `provider` and `model`. -/
def effectiveWireApi (cfg : ModelConfig) : WireApi :=
  match cfg.wireApi with
  | .auto => cfg.provider.defaultWireApi cfg.model
  | api   => api

/-- Effective model identifier string for the configured `provider`. -/
def effectiveModelId (cfg : ModelConfig) : String :=
  cfg.provider.resolveModelId cfg.model

end ModelConfig
end Leanllm
