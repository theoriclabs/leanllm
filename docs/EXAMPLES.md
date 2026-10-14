# Advanced Examples

This guide covers multi-step and production patterns in `leanllm`. For basic usage (single prompts, multi-turn chat, SSE streaming, and configuration), see the main [README](../README.md). For internal design and wire codecs, see [ARCHITECTURE.md](ARCHITECTURE.md).

## Contents

- [Multi-turn tool-calling agent loop](#multi-turn-tool-calling-agent-loop)
- [Multimodal vision inputs (URL and base64 images)](#multimodal-vision-inputs-url-and-base64-images)
- [Multi-model workflows in a single session](#multi-model-workflows-in-a-single-session)
- [OpenRouter fallbacks, provider ordering, and sorting](#openrouter-fallbacks-provider-ordering-and-sorting)
- [Cloud enterprise providers and local servers](#cloud-enterprise-providers-and-local-servers)
- [Reasoning traces and prompt cache metrics](#reasoning-traces-and-prompt-cache-metrics)
- [Rate-limit retry and structured error recovery](#rate-limit-retry-and-structured-error-recovery)
- [Manual `LeanHttp.Session` lifecycle](#manual-leanhttpsession-lifecycle)

---

## Multi-turn tool-calling agent loop

When the model decides to invoke one or more tools, `resp.stopReason` is `.toolUse` (or `resp.toolCalls` is non-empty) and `resp.message` contains the assistant's `.toolCall` blocks. Append `resp.message` followed by a `Message.toolResult` for each call, then invoke `completion` again until the model finishes its turn.

```lean
import Leanllm

open Leanllm Lean

def weatherTool : ToolSpec := {
  name        := "get_weather"
  description := "Get the current weather for a city"
  parameters  := Json.mkObj [
    ("type", "object"),
    ("properties", Json.mkObj [
      ("city", Json.mkObj [
        ("type", "string"),
        ("description", "City name, e.g. Tokyo")
      ])
    ]),
    ("required", toJson ["city"])
  ]
}

def timeTool : ToolSpec := {
  name        := "get_local_time"
  description := "Get the local time in a city"
  parameters  := Json.mkObj [
    ("type", "object"),
    ("properties", Json.mkObj [
      ("city", Json.mkObj [("type", "string")])
    ]),
    ("required", toJson ["city"])
  ]
}

/-- Execute a tool call locally and return a `Message.toolResult`. -/
def dispatchTool (tc : ToolCall) : IO Message := do
  let city := (tc.arguments.getObjValAs? String "city").toOption.getD "Unknown"
  match tc.name with
  | "get_weather" =>
    return Message.toolResult tc.id s!"72°F and clear skies in {city}"
  | "get_local_time" =>
    return Message.toolResult tc.id s!"14:30 local time in {city}"
  | other =>
    return Message.toolResult tc.id s!"Unknown tool: {other}" (isError := true)

/-- Run an agent loop for up to `maxSteps` rounds of tool execution. -/
def agentLoop (history : List Message) (maxSteps : Nat := 5) : LLMM String := do
  let mut msgs := history
  for _ in [:maxSteps] do
    let resp ← completion msgs
    if resp.toolCalls.isEmpty then
      return resp.text
    -- Append the assistant's tool-call turn, then each tool result
    msgs := msgs ++ [resp.message]
    for tc in resp.toolCalls do
      let resultMsg ← liftM (m := IO) (dispatchTool tc)
      msgs := msgs ++ [resultMsg]
  LLMM.throw (.invalidRequest (.malformedParameters "Exceeded max tool steps") "Max steps reached")

def main : IO Unit := do
  let cfg : ModelConfig := {
    model     := .claude .haiku55
    tools     := [weatherTool, timeTool]
    maxTokens := some 512
  }
  let initial := [
    Message.system "Use the provided tools to answer the user's question.",
    Message.user "What is the weather and local time in Tokyo?"
  ]
  match ← LLMM.run cfg (agentLoop initial) with
  | .ok answer => IO.println answer
  | .error err => IO.eprintln s!"Agent failed: {repr err}"
```

Because `Codec` translates `resp.message` and `Message.toolResult` into each wire dialect's native schema (`tool_use` / `tool_result` on Anthropic and Bedrock Converse, `tool_calls` / `"role": "tool"` on OpenAI Chat, `function_call` / `function_call_output` on OpenAI Responses, and `functionCall` / `functionResponse` on Gemini), the exact same `agentLoop` runs unchanged across all providers.

---

## Multimodal vision inputs (URL and base64 images)

Construct a `Message` with `ContentPart.image` alongside `ContentPart.text`. `ImageSource` supports both remote URLs (`.url`) and inline base64 data (`.base64 mediaType data`):

```lean
import Leanllm

open Leanllm

/-- Send an image URL and a question in a single user message. -/
def describeImageUrl (imageUrl : String) : LLMM String := do
  let userMsg : Message := {
    role    := .user
    content := [
      .image (.url imageUrl),
      .text "Describe the key elements of this diagram in three bullet points."
    ]
  }
  let resp ← completion [userMsg]
  return resp.text

/-- Send a base64-encoded PNG image with a prompt. -/
def analyzeBase64Png (base64Png : String) : LLMM String := do
  let userMsg : Message := {
    role    := .user
    content := [
      .image (.base64 "image/png" base64Png),
      .text "Extract all visible text from this screenshot."
    ]
  }
  let resp ← completion [userMsg]
  return resp.text
```

How each codec serializes `ImageSource`:
- `.anthropicMessages` emits `{"type": "image", "source": {"type": "url", ...}}` or `{"type": "base64", "media_type": ..., "data": ...}`.
- `.openaiChat` emits `{"type": "image_url", "image_url": {"url": ...}}`, formatting `.base64 mime data` as a `data:<mime>;base64,<data>` URI.
- `.geminiGenerate` emits `{"fileData": {"fileUri": ...}}` for URLs and `{"inlineData": {"mimeType": ..., "data": ...}}` for base64 payloads.

---

## Multi-model workflows in a single session

`LLMM` is a `ReaderT LLMContext (ExceptT LLMError IO)` monad where `LLMContext` holds both the `ModelConfig` and the active `LeanHttp.Session`. You can use `LLMM.withModel`, `LLMM.withProvider`, `LLMM.withWireApi`, and `LLMM.withConfig` to route sub-tasks to different models or providers while reusing the same underlying HTTP session:

```lean
import Leanllm

open Leanllm

def draftAndCritique (topic : String) : LLMM (String × String) := do
  -- Step 1: Fast draft with Gemini 3.5 Flash (Direct Google API)
  let draft ←
    LLMM.withProvider .direct <|
    LLMM.withModel (.gemini .flash35) <|
    LLMM.withConfig (fun c => { c with temperature := some 0.7, maxTokens := some 512 }) <|
      ask s!"Write a short technical paragraph explaining {topic}."

  -- Step 2: Review using OpenAI gpt-5.4 over the Responses API on OpenRouter
  let review ←
    LLMM.withProvider (.openRouter { appName := some "leanllm-pipeline" }) <|
    LLMM.withModel (.openai .gpt54) <|
    LLMM.withWireApi .openaiResponses <|
      completion [
        Message.system "Check the draft for technical precision and suggest one improvement.",
        Message.user draft
      ]

  return (draft, review.text)

def main : IO Unit := do
  let defaultCfg : ModelConfig := { model := .claude .sonnet55, maxTokens := some 512 }
  match ← LLMM.run defaultCfg (draftAndCritique "dependent types in Lean 4") with
  | .ok (draft, critique) =>
    IO.println s!"--- Draft ---\n{draft}\n\n--- Critique ---\n{critique}"
  | .error err =>
    IO.eprintln s!"Pipeline error: {repr err}"
```

---

## OpenRouter fallbacks, provider ordering, and sorting

`Provider.openRouter` accepts an `OpenRouterConfig` that maps directly to OpenRouter's routing parameters (`models`, `provider.order`, `provider.allow_fallbacks`, `provider.sort`, and optional `HTTP-Referer` / `X-Title` headers):

```lean
import Leanllm

open Leanllm

def resilientConfig : ModelConfig := {
  model    := .claude .sonnet55
  provider := .openRouter {
    siteUrl        := some "https://example.com"
    appName        := some "leanllm-service"
    -- If Claude Sonnet 5.5 is unavailable, OpenRouter tries these in order:
    fallbacks      := [.claude .haiku55, .gemini .flash35, .openai .gpt4oMini]
    -- Prefer specific upstream infrastructure providers:
    order          := ["Anthropic", "Google", "OpenAI"]
    allowFallbacks := true
    -- Route to the lowest-latency provider:
    sort           := some .latency
  }
  maxTokens := some 512
}
```

You can also pair `.openRouter` with `wireApi := .openaiResponses` (hitting `https://openrouter.ai/api/v1/responses`) or `wireApi := .anthropicMessages` (hitting `https://openrouter.ai/api/v1/messages`).

---

## Cloud enterprise providers and local servers

### AWS Bedrock (Converse and Invoke APIs)

By default, `Provider.bedrock` uses `WireApi.bedrockConverse` (`/model/{id}/converse`) and prepends `us.` for cross-region inference (`us.anthropic.claude-sonnet-5-5-v1:0`). Set `AWS_BEARER_TOKEN_BEDROCK` or `BEDROCK_API_KEY`:

```lean
import Leanllm

open Leanllm

-- AWS Bedrock Converse API (default)
def bedrockConverseCfg : ModelConfig := {
  model     := .claude .sonnet55
  provider  := .bedrock { region := "us-west-2", crossRegion := true }
  maxTokens := some 512
}

-- AWS Bedrock native Anthropic Invoke payload (/model/{id}/invoke)
def bedrockInvokeCfg : ModelConfig := {
  model     := .claude .haiku55
  provider  := .bedrock { region := "us-east-1", crossRegion := true }
  wireApi   := .bedrockInvoke
  maxTokens := some 512
}
```

### Azure OpenAI

`Provider.azure` targets `https://<endpoint>/openai/deployments/<deployment>/chat/completions?api-version=<apiVersion>` (or `/openai/v1/responses` when `wireApi := .openaiResponses`) and authenticates with the `api-key` header from `AZURE_OPENAI_API_KEY` or `OPENAI_API_KEY`:

```lean
def azureCfg : ModelConfig := {
  model    := .openai .gpt4o
  provider := .azure {
    endpoint   := "https://my-org.openai.azure.com"
    deployment := some "gpt-4o-prod"
    apiVersion := "2024-10-21"
  }
  maxTokens := some 512
}
```

### Google Cloud Vertex AI

`Provider.vertex` supports both Anthropic Claude models (`:rawPredict` with `.anthropicMessages`) and Google Gemini models (`:generateContent` with `.geminiGenerate`), authenticating via `VERTEX_API_KEY` or `GOOGLE_ACCESS_TOKEN`:

```lean
def vertexClaudeCfg : ModelConfig := {
  model    := .claude .sonnet55
  provider := .vertex { projectId := "my-gcp-project", location := "us-east5" }
  maxTokens := some 512
}

def vertexGeminiCfg : ModelConfig := {
  model    := .gemini .pro25
  provider := .vertex { projectId := "my-gcp-project", location := "us-central1" }
  maxTokens := some 512
}
```

### Local servers (Ollama, vLLM, llama.cpp)

`Provider.custom` accepts any base URL and does not require an API key if neither `cfg.apiKey` nor `LLM_API_KEY` / `OPENAI_API_KEY` is set:

```lean
def ollamaCfg : ModelConfig := {
  model    := .openai (.custom "qwen2.5-coder:32b")
  provider := .custom "http://127.0.0.1:11434/v1"
  wireApi  := .openaiChat
}
```

---

## Reasoning traces and prompt cache metrics

Reasoning models (Claude with extended thinking, OpenAI `o3` / `o4-mini` / `gpt-5.4`, Gemini thinking models, and Bedrock Converse reasoning) emit thought blocks that `leanllm` normalizes into `ContentPart.thinking`. Use `resp.thinking` and `resp.usage` to inspect them:

```lean
import Leanllm

open Leanllm

def inspectReasoning : IO Unit := do
  let cfg : ModelConfig := {
    model     := .openai .o4Mini
    maxTokens := some 1024
  }
  match ← LLMM.run cfg (completion [Message.user "Prove that the square root of 2 is irrational."]) with
  | .ok resp =>
    for thought in resp.thinking do
      IO.println s!"[Thinking]\n{thought}\n"
    IO.println s!"[Answer]\n{resp.text}"
    if let some u := resp.usage then
      IO.println s!"Tokens: in={u.inputTokens}, out={u.outputTokens}, reasoning={u.reasoningTokens.getD 0}, cacheRead={u.cacheReadTokens.getD 0}"
  | .error err =>
    IO.eprintln s!"Error: {repr err}"
```

---

## Rate-limit retry and structured error recovery

`Transport.fromHttpStatus` parses HTTP `429` responses along with the `Retry-After` header (converted to milliseconds in `RateLimitErrorKind`) and maps `503` / `529` responses to `.provider .overloaded`. You can pattern-match on `LLMError` to implement targeted backoff or provider fallback:

```lean
import Leanllm

open Leanllm

/-- Retry a computation on rate-limit or overload errors, honouring Retry-After when present. -/
def withRetry (maxAttempts : Nat) (defaultDelayMs : UInt32 := 1000) (act : LLMM α) : LLMM α := do
  let ctx ← read
  let mut attempt := 0
  repeat
    attempt := attempt + 1
    match ← liftM (m := IO) (ExceptT.run (ReaderT.run act ctx)) with
    | .ok val => return val
    | .error err =>
      if attempt >= maxAttempts then
        LLMM.throw err
      match err with
      | .rateLimit (.requestsPerMinute retryMs) _
      | .rateLimit (.tokensPerMinute retryMs) _ =>
        let waitMs := (retryMs.map Nat.toUInt32).getD defaultDelayMs
        liftM (m := IO) (IO.sleep waitMs)
      | .provider .overloaded _
      | .provider .badGateway _
      | .network .timeout _ =>
        liftM (m := IO) (IO.sleep defaultDelayMs)
      | other =>
        -- Non-retryable error (.auth, .invalidRequest, .parse, .contentBlocked)
        LLMM.throw other
   unreachable!

/-- Fall back to OpenRouter if the direct provider fails. -/
def withOpenRouterFallback (act : LLMM α) : LLMM α := do
  let ctx ← read
  match ← liftM (m := IO) (ExceptT.run (ReaderT.run act ctx)) with
  | .ok val  => return val
  | .error _ => LLMM.withProvider (.openRouter {}) act
```

---

## Manual `LeanHttp.Session` lifecycle

`LLMM.run` creates and closes a `LeanHttp.Session` for a single `LLMM` block. If your application runs a long-lived service and wants to share a session across multiple independent `IO` actions, manage the session directly with `LeanHttp.Session.new` and `LLMM.runWithSession` (or `Leanllm.callWithSession`):

```lean
import LeanHttp
import Leanllm

open Leanllm

def runServerBatch (prompts : List String) : IO Unit := do
  let cfg : ModelConfig := { model := .claude .haiku55, maxTokens := some 128 }
  match ← LeanHttp.Session.new with
  | .error httpErr =>
    IO.eprintln s!"Failed to initialize HTTP session: {httpErr}"
  | .ok session =>
    try
      for p in prompts do
        match ← LLMM.runWithSession cfg session (ask p) with
        | .ok reply  => IO.println reply
        | .error err => IO.eprintln s!"Call error: {repr err}"
    finally
      session.close
```
