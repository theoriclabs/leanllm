import Leanllm

open Leanllm

def main : IO Unit := do
  let baseCfg : ModelConfig := { model := .claude .sonnet4 }
  let orCfg   : ModelConfig := { baseCfg with provider := .openRouter {} }
  let brCfg   : ModelConfig := { baseCfg with provider := .bedrock { region := "us-west-2" } }
  IO.println s!"Direct:     id={baseCfg.effectiveModelId} api={repr baseCfg.effectiveWireApi}"
  IO.println s!"OpenRouter: id={orCfg.effectiveModelId} api={repr orCfg.effectiveWireApi}"
  IO.println s!"Bedrock:    id={brCfg.effectiveModelId} api={repr brCfg.effectiveWireApi}"
