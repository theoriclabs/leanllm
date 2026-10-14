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

private def bashTool : ToolSpec := {
  name        := "bash"
  description := "Run a shell command"
  parameters  := Lean.Json.mkObj [
    ("type", "object"),
    ("properties", Lean.Json.mkObj [
      ("command", Lean.Json.mkObj [
        ("type", "string"),
        ("description", "Shell command to run")
      ])
    ]),
    ("required", Lean.toJson ["command"])
  ]
}

private def runTest (label : String) (cfg : ModelConfig) (msgs : List Message) : IO (Option LLMResponse) := do
  IO.println s!"=== {label} (model={cfg.effectiveModelId}, auth={repr cfg.auth}) ==="
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

private def runStreamTest (label : String) (cfg : ModelConfig) (msgs : List Message) : IO Unit := do
  IO.println s!"=== {label} ==="
  IO.print "  Streamed:   "
  let chunksRef ← IO.mkRef 0
  match ← LLMM.run cfg (completionStream msgs fun delta => do
    chunksRef.modify (· + 1)
    IO.print delta) with
  | .ok _ =>
    let n ← chunksRef.get
    IO.println s!"\n  Chunks:     {n}"
  | .error err =>
    IO.println s!"\n  ERROR:      {repr err}"

def main : IO Unit := do
  let promptMsgs := [
    Message.system "Reply in at most 8 words.",
    Message.user "What is 2 + 2?"
  ]
  let toolMsgs := [
    Message.user "What is the weather in Tokyo? Call the get_weather tool."
  ]
  let bashMsgs := [
    Message.user "List files in /tmp using the bash tool (`ls /tmp`)."
  ]

  -- 0. Test OpenAI OAuth JWT Claim Extraction & Live Token Refresh (no hardcoded client_id)
  IO.println "=== 0. OpenAI OAuth JWT Claim Extraction & Live Token Refresh ==="
  match ← Auth.OpenAI.loadFromCodexCliFile with
  | none => IO.println "  ERROR: ~/.codex/auth.json not found"
  | some diskCreds =>
    IO.println s!"  Loaded from:      {repr diskCreds.source}"
    IO.println s!"  JWT client_id?:   {diskCreds.clientId.isSome} (len={diskCreds.clientId.getD "" |>.length})"
    IO.println s!"  JWT account_id?:  {diskCreds.accountId.isSome}"
    IO.println s!"  JWT expires_at:   {repr diskCreds.expiresAtMs}"
    match ← ExceptT.run (Auth.OpenAI.refreshCredentials diskCreds) with
    | .ok refreshed =>
      IO.println s!"  Live Refresh:     OK (new expires_at={repr refreshed.expiresAtMs}, client_id preserved={refreshed.clientId == diskCreds.clientId})"
    | .error err =>
      IO.println s!"  Live Refresh ERR: {repr err}"

  -- 1. OpenAI Subscription OAuth: Completion, Multi-turn Tool Call + Tool Result, and SSE Stream
  let _ ← runTest "1a. OpenAI Subscription OAuth (gpt-5.6-sol)"
    { model := .openai .gpt56Sol, auth := .oauth none }
    promptMsgs

  let _ ← runTest "1b. OpenAI Subscription OAuth (gpt-6-astra)"
    { model := .openai .gpt6Astra, auth := .oauth none }
    promptMsgs

  if let some toolResp ← runTest "1c. OpenAI Subscription OAuth Tool Call (gpt-5.6-sol)"
    { model := .openai .gpt56Sol, auth := .oauth none, tools := [weatherTool] }
    toolMsgs then
    if let some tc := toolResp.toolCalls.head? then
      let followUpMsgs := toolMsgs ++ [
        toolResp.message,
        Message.toolResult tc.id "{\"temp_c\": 22, \"condition\": \"Sunny\"}"
      ]
      let _ ← runTest "1d. OpenAI Subscription OAuth Tool Result Continuation (gpt-5.6-sol)"
        { model := .openai .gpt56Sol, auth := .oauth none, tools := [weatherTool] }
        followUpMsgs

  runStreamTest "1e. OpenAI Subscription OAuth SSE Stream (gpt-5.6-sol)"
    { model := .openai .gpt56Sol, auth := .oauth none }
    [Message.user "Count from 1 to 5 separated by spaces."]

  -- 2. Claude Subscription OAuth
  let _ ← runTest "2a. Claude Subscription OAuth (Claude Sonnet 5.5)"
    { model := .claude .sonnet55, auth := .oauth none, maxTokens := some 64 }
    promptMsgs

  let _ ← runTest "2b. Claude Subscription OAuth + Tool Call (bash <-> Bash mapping)"
    { model := .claude .haiku55, auth := .oauth none, tools := [bashTool], maxTokens := some 128 }
    bashMsgs

  -- 3. AuthMode.auto fallback (no API keys in env)
  let _ ← runTest "3a. AuthMode.auto on OpenAI (gpt-5.5)"
    { model := .openai .gpt55, auth := .auto }
    promptMsgs
