# leanllm

**A unified LLM client library for Lean 4, backed by [LeanHttp](https://github.com/theoriclabs/leanhttp).**

[![Lean](https://img.shields.io/badge/Lean-v4.34.1-555555)](https://lean-lang.org)

`leanllm` provides a single typed interface for Anthropic Claude, OpenAI, and Google Gemini models across direct vendor APIs, OpenRouter, AWS Bedrock, Azure OpenAI, Google Vertex AI, and local OpenAI-compatible servers.

## Install

Add `leanllm` to your `lakefile.toml`:

```toml
[[require]]
name = "leanllm"
git = "https://github.com/theoriclabs/leanllm"
rev = "main"
```

Requires Lean `v4.34.1` and `libcurl` at runtime (loaded dynamically by `leanhttp`).

## Quick start

### 1. Ask a single question

Set `ANTHROPIC_API_KEY` in your environment and run a prompt with `LLMM.run` and `ask`:

```lean
import Leanllm

open Leanllm

def main : IO Unit := do
  let cfg : ModelConfig := {
    model     := .claude .sonnet55
    maxTokens := some 256
  }
  match ← LLMM.run cfg (ask "Explain the Curry-Howard correspondence in two sentences.") with
  | .ok reply  => IO.println reply
  | .error err => IO.eprintln s!"Error: {repr err}"
```

### 2. Multi-turn conversation

Pass a list of `Message` values to `completion` to inspect the full `LLMResponse` (assistant text, stop reason, and token usage):

```lean
import Leanllm

open Leanllm

def chat : IO Unit := do
  let cfg : ModelConfig := {
    model       := .openai .gpt4oMini
    temperature := some 0.2
    maxTokens   := some 256
  }
  let messages := [
    Message.system "Reply in at most two sentences.",
    Message.user "What is Lean 4?",
    Message.assistant "Lean 4 is a functional programming language and interactive theorem prover.",
    Message.user "What logic is its kernel based on?"
  ]
  match ← LLMM.run cfg (completion messages) with
  | .ok resp =>
    IO.println resp.text
    if let some u := resp.usage then
      IO.println s!"(tokens: {u.inputTokens} in, {u.outputTokens} out)"
  | .error err =>
    IO.eprintln s!"Error: {repr err}"
```

### 3. Stream tokens over Server-Sent Events

Use `askStream` (or `completionStream` for multi-turn history) to receive each text chunk as it arrives:

```lean
import Leanllm

open Leanllm

def stream : IO Unit := do
  let cfg : ModelConfig := { model := .gemini .flash35, maxTokens := some 256 }
  match ← LLMM.run cfg (askStream "Count from 1 to 5." IO.print) with
  | .ok _      => IO.println ""
  | .error err => IO.eprintln s!"\nStream error: {repr err}"
```

### 4. Switch models and providers

`ModelConfig` separates the model (`model`), where it is hosted (`provider`), and which JSON schema goes over the wire (`wireApi`). Your calling code stays identical when you switch providers:

```lean
import Leanllm

open Leanllm

-- Direct Google Gemini (reads GEMINI_API_KEY or GOOGLE_API_KEY)
def geminiCfg : ModelConfig :=
  { model := .gemini .flash35, maxTokens := some 256 }

-- Any model via OpenRouter (reads OPENROUTER_API_KEY)
def openRouterCfg : ModelConfig :=
  { model    := .claude .haiku55
    provider := .openRouter { appName := some "my-lean-app" } }

-- OpenAI /v1/responses endpoint instead of /v1/chat/completions
def responsesCfg : ModelConfig :=
  { model   := .openai .gpt54
    wireApi := .openaiResponses }

-- Local Ollama or vLLM server (no API key required)
def localCfg : ModelConfig :=
  { model    := .openai (.custom "qwen2.5-coder:7b")
    provider := .custom "http://localhost:11434/v1" }
```

### 5. Call a tool

Declare a tool with `ToolSpec` and read `resp.toolCalls` from the response:

```lean
import Leanllm

open Leanllm

def weatherTool : ToolSpec := {
  name        := "get_weather"
  description := "Get the current weather for a city"
  parameters  := Lean.Json.mkObj [
    ("type", "object"),
    ("properties", Lean.Json.mkObj [
      ("city", Lean.Json.mkObj [("type", "string")])
    ]),
    ("required", Lean.toJson ["city"])
  ]
}

def checkWeather : IO Unit := do
  let cfg : ModelConfig := {
    model     := .claude .haiku55
    tools     := [weatherTool]
    maxTokens := some 256
  }
  let msgs := [Message.user "What is the weather in Tokyo?"]
  match ← LLMM.run cfg (completion msgs) with
  | .ok resp =>
    for tc in resp.toolCalls do
      IO.println s!"Call {tc.name} with {tc.arguments.compress}"
  | .error err =>
    IO.eprintln s!"Error: {repr err}"
```

### 6. One-shot call without `LLMM`

If you only need a single request and do not want the `LLMM` reader monad, call `Leanllm.call` or `Leanllm.callStream` directly in `ExceptT LLMError IO`:

```lean
import Leanllm

open Leanllm

def oneShot : IO Unit := do
  let cfg : ModelConfig := { model := .claude .haiku55, maxTokens := some 64 }
  match ← (Leanllm.call cfg [Message.user "What is 2 + 2?"]).run with
  | .ok resp   => IO.println resp.text
  | .error err => IO.eprintln s!"Error: {repr err}"
```

## Supported models and providers

### Models

| Family | Constructor | Built-in variants |
| --- | --- | --- |
| Anthropic Claude | `Model.claude` | `.opus55`, `.sonnet55`, `.haiku55`, `.fable51`, `.opus5`, `.sonnet5`, `.opus45`, `.sonnet45`, `.haiku45`, `.custom id` |
| OpenAI | `Model.openai` | `.gpt56Sol`, `.gpt54`, `.gpt4o`, `.gpt4oMini`, `.o3`, `.o4Mini`, `.custom id` |
| Google Gemini | `Model.gemini` | `.argon4`, `.flash38`, `.flash35`, `.pro25`, `.flash25`, `.custom id` |

### Providers and environment variables

When `ModelConfig.apiKey` is `none`, `leanllm` looks up the provider's environment variable:

| Provider | Environment variables | Default `WireApi` |
| --- | --- | --- |
| `.direct` | `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY` (or `GOOGLE_API_KEY`) | `.anthropicMessages` / `.openaiChat` / `.geminiGenerate` |
| `.openRouter cfg` | `OPENROUTER_API_KEY` | `.openaiChat` |
| `.bedrock cfg` | `AWS_BEARER_TOKEN_BEDROCK`, `BEDROCK_API_KEY` | `.bedrockConverse` |
| `.azure cfg` | `AZURE_OPENAI_API_KEY`, `OPENAI_API_KEY` | `.openaiChat` |
| `.vertex cfg` | `VERTEX_API_KEY`, `GOOGLE_ACCESS_TOKEN` | `.anthropicMessages` (Claude) / `.geminiGenerate` (Gemini) |
| `.custom baseUrl` | `LLM_API_KEY`, `OPENAI_API_KEY` (optional) | `.openaiChat` |

## Documentation

See [docs/EXAMPLES.md](docs/EXAMPLES.md) for multi-turn tool-calling agent loops, vision inputs (URL and base64 images), multi-model workflows in a single `LLMM` session, OpenRouter fallback and sorting configuration, AWS Bedrock / Azure OpenAI / Vertex AI setup, reasoning traces, and rate-limit retry recovery.

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for the module dependency graph, `Provider × Model × WireApi` routing rules, the 5-stage HTTP/SSE execution pipeline, wire codec internals, and `LLMError` mapping.

## Building and running the test suite

```bash
lake build
```

`Main.lean` runs 8 end-to-end integration checks across direct Anthropic, direct Gemini, and OpenRouter (synchronous completions, tool calls, the OpenAI Responses wire API, and SSE streaming):

```bash
ANTHROPIC_API_KEY=... GEMINI_API_KEY=... OPENROUTER_API_KEY=... lake exe leanllm
```
