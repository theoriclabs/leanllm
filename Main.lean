import Leanllm

open Leanllm

private def weatherTool : ToolSpec := {
  name        := "get_weather"
  description := "Get the current weather for a city"
  parameters  := Lean.Json.mkObj [
    ("type", "object"),
    ("properties", Lean.Json.mkObj [
      ("city", Lean.Json.mkObj [
        ("type", "string"),
        ("description", "City name, e.g. San Francisco")
      ])
    ]),
    ("required", Lean.toJson ["city"])
  ]
}

private def runTest (label : String) (cfg : ModelConfig) (msgs : List Message) : IO (Option LLMResponse) := do
  IO.println s!"=== {label} (model={cfg.effectiveModelId}, provider={repr cfg.provider}) ==="
  match ← LLMM.run cfg (completion msgs) with
  | .ok resp =>
    IO.println s!"  Status:     OK (id={resp.id})"
    IO.println s!"  StopReason: {repr resp.stopReason}"
    if !resp.text.isEmpty then
      IO.println s!"  Text:       {resp.text}"
    if !resp.toolCalls.isEmpty then
      for tc in resp.toolCalls do
        IO.println s!"  ToolCall:   {tc.name}({tc.arguments.compress}) [id={tc.id}]"
    if let some u := resp.usage then
      IO.println s!"  Usage:      in={u.inputTokens} out={u.outputTokens} cached={repr u.cacheReadTokens}"
    return some resp
  | .error err =>
    IO.println s!"  ERROR:      {repr err}"
    return none

def main : IO Unit := do
  let promptMsgs := [
    Message.system "Reply in at most 8 words.",
    Message.user "What is 2 + 2?"
  ]
  let toolMsgs := [
    Message.user "What is the weather in Tokyo? Call the get_weather tool."
  ]

  -- 1. Verify Direct & Coding-Plan Endpoint Resolution for DeepSeek, Kimi, GLM, Qwen, MiniMax, Grok, Mistral
  IO.println "=== 1. Direct & Coding-Plan Endpoint Resolution ==="
  for (name, cfg, auth) in [
    ("DeepSeek Direct",      { model := .deepseek .v4Flash : ModelConfig },                       ResolvedAuth.apiKey "sk-ds"),
    ("Kimi Code Direct",     { model := .kimi .forCoding : ModelConfig },                         ResolvedAuth.apiKey "sk-kimi-123"),
    ("Moonshot Kimi Direct", { model := .kimi .k2_5 : ModelConfig },                              ResolvedAuth.apiKey "sk-ms-123"),
    ("Z.AI GLM Coding",      { model := .glm .glm5_1 : ModelConfig },                             ResolvedAuth.apiKey "sk-zai"),
    ("Alibaba Qwen Coding",  { model := .qwen .coderPlus : ModelConfig },                         ResolvedAuth.apiKey "sk-bailian"),
    ("MiniMax Global",       { model := .minimax .m2_7 : ModelConfig },                           ResolvedAuth.apiKey "sk-mm"),
    ("MiniMax China (cp)",   { model := .minimax .m2_7 : ModelConfig },                           ResolvedAuth.apiKey "sk-cp-mm"),
    ("xAI Grok Direct",      { model := .grok .grok4_6 : ModelConfig },                           ResolvedAuth.apiKey "xai-123"),
    ("Mistral Direct",       { model := .mistral .devstral : ModelConfig },                       ResolvedAuth.apiKey "mis-123")
  ] do
    let url := Transport.resolveEndpointUrlWithAuth cfg auth
    IO.println s!"  {name}: model={cfg.effectiveModelId} -> {url}"

  -- 2. Live Calls across DeepSeek, Kimi, GLM, Qwen, MiniMax, Grok, and Mistral (plus OpenAI & Claude OAuth)
  let _ ← runTest "2a. DeepSeek V3.2 (via OpenRouter)"
    { model := .deepseek .v3_2, provider := .openRouter { appName := some "leanllm" }, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2b. Kimi K2.5 + Tool Call (via OpenRouter)"
    { model := .kimi .k2_5, provider := .openRouter { appName := some "leanllm" }, tools := [weatherTool], maxTokens := some 128 }
    toolMsgs

  let _ ← runTest "2c. Z.AI GLM-5 (via OpenRouter)"
    { model := .glm .glm5, provider := .openRouter { appName := some "leanllm" }, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2d. Qwen3 Coder Next (via OpenRouter)"
    { model := .qwen .coderNext, provider := .openRouter { appName := some "leanllm" }, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2e. MiniMax M2.5 (via OpenRouter)"
    { model := .minimax .m2_5, provider := .openRouter { appName := some "leanllm" }, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2f. xAI Grok Direct (grok-4.6)"
    { model := .grok .grok4_6, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2g. Mistral Large (via OpenRouter)"
    { model := .mistral .large4, provider := .openRouter { appName := some "leanllm" }, maxTokens := some 128 }
    promptMsgs

  let _ ← runTest "2h. OpenAI Subscription OAuth (gpt-5.6-sol)"
    { model := .openai .gpt5_6Sol, auth := .oauth none }
    promptMsgs

  let _ ← runTest "2i. Claude Subscription OAuth (Claude Haiku 5.5)"
    { model := .claude .haiku5_5, auth := .oauth none, maxTokens := some 64 }
    promptMsgs
