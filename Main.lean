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

private def runTest (label : String) (cfg : ModelConfig) (msgs : List Message) : IO Unit := do
  IO.println s!"=== {label} (model={cfg.effectiveModelId}, wireApi={repr cfg.effectiveWireApi}) ==="
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
      IO.println s!"  Usage:      in={u.inputTokens} out={u.outputTokens}"
  | .error err =>
    IO.println s!"  ERROR:      {repr err}"

def main : IO Unit := do
  let promptMsgs := [
    Message.system "Reply in at most 8 words.",
    Message.user "What is 2 + 2?"
  ]
  let toolMsgs := [
    Message.user "What is the weather in Tokyo? Call the get_weather tool."
  ]

  -- 1. Direct Anthropic (Claude Sonnet 5.5)
  runTest "1. Direct Anthropic (Claude Sonnet 5.5)"
    { model := .claude .sonnet55, maxTokens := some 64 }
    promptMsgs

  -- 2. Direct Anthropic + Tool Call (Claude Haiku 5.5)
  runTest "2. Direct Anthropic Tool Call (Claude Haiku 5.5)"
    { model := .claude .haiku55, tools := [weatherTool], maxTokens := some 128 }
    toolMsgs

  -- 3. Direct Google Gemini (gemini-3.5-flash)
  runTest "3. Direct Google Gemini (gemini-3.5-flash)"
    { model := .gemini .flash35, maxTokens := some 128 }
    promptMsgs

  -- 4. Direct Google Gemini + Tool Call (gemini-3.5-flash)
  runTest "4. Direct Google Gemini Tool Call (gemini-3.5-flash)"
    { model := .gemini .flash35, tools := [weatherTool], maxTokens := some 128 }
    toolMsgs

  -- 5. OpenRouter (Claude Haiku 5.5 + Tool Call)
  runTest "5. OpenRouter + Tool Call (Claude Haiku 5.5)"
    { model     := .claude .haiku55
      provider  := .openRouter { appName := some "leanllm-test" }
      tools     := [weatherTool]
      maxTokens := some 128 }
    toolMsgs

  -- 6. OpenRouter (OpenAI GPT-4o-mini via OpenRouter)
  runTest "6. OpenRouter (OpenAI gpt-4o-mini)"
    { model     := .openai .gpt4oMini
      provider  := .openRouter { appName := some "leanllm-test" }
      maxTokens := some 64 }
    promptMsgs

  -- 7. OpenRouter (OpenAI Responses WireApi via OpenRouter)
  runTest "7. OpenRouter with WireApi.openaiResponses (gpt-4o-mini)"
    { model     := .openai .gpt4oMini
      provider  := .openRouter { appName := some "leanllm-test" }
      wireApi   := .openaiResponses
      maxTokens := some 64 }
    promptMsgs

  -- 8. Live SSE Streaming via LeanHttp v0.6.0 (Anthropic, Gemini, OpenRouter)
  for (label, cfg) in [
    ("8a. SSE Stream Direct Anthropic (Claude Haiku 5.5)",
      { model := .claude .haiku55, maxTokens := some 64 : ModelConfig }),
    ("8b. SSE Stream Direct Gemini (gemini-3.5-flash)",
      { model := .gemini .flash35, maxTokens := some 128 : ModelConfig }),
    ("8c. SSE Stream OpenRouter (gpt-4o-mini)",
      { model := .openai .gpt4oMini, provider := .openRouter { appName := some "leanllm-test" }, maxTokens := some 64 : ModelConfig })
  ] do
    IO.println s!"=== {label} ==="
    IO.print "  Streamed:   "
    let chunksRef ← IO.mkRef 0
    match ← LLMM.run cfg (completionStream promptMsgs fun delta => do
      chunksRef.modify (· + 1)
      IO.print delta) with
    | .ok _ =>
      let n ← chunksRef.get
      IO.println s!"\n  Chunks:     {n}"
    | .error err =>
      IO.println s!"\n  ERROR:      {repr err}"
