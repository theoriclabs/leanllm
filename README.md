# leanllm

**A unified LLM client library for Lean 4, backed by [LeanHttp](https://github.com/theoriclabs/leanhttp).**

[![Release](https://img.shields.io/badge/release-v0.2.0-3178c6)](https://github.com/theoriclabs/leanllm/releases)
[![Lean](https://img.shields.io/badge/Lean-v4.34.1-555555)](https://lean-lang.org)

`leanllm` provides a single typed interface for Anthropic Claude, OpenAI, Google Gemini, DeepSeek, Kimi/Moonshot, Z.AI GLM, Qwen, MiniMax, xAI Grok, and Mistral models across direct vendor APIs, subscription OAuth sessions (Claude Code, OpenAI Codex/ChatGPT, Gemini Code Assist, Antigravity, GitHub Copilot, Grok Build), OpenRouter, AWS Bedrock, Azure OpenAI, Google Vertex AI, and local OpenAI-compatible servers.

## Install

Add `leanllm` to your `lakefile.toml`:

```toml
[[require]]
name = "leanllm"
git = "https://github.com/theoriclabs/leanllm"
rev = "v0.2.0"
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
    model     := .claude .sonnet5_5
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
  let cfg : ModelConfig := { model := .gemini .flash3_5, maxTokens := some 256 }
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
  { model := .gemini .flash3_5, maxTokens := some 256 }

-- Any model via OpenRouter (reads OPENROUTER_API_KEY)
def openRouterCfg : ModelConfig :=
  { model    := .claude .haiku5_5
    provider := .openRouter { appName := some "my-lean-app" } }

-- OpenAI /v1/responses endpoint instead of /v1/chat/completions
def responsesCfg : ModelConfig :=
  { model   := .openai .gpt5_4
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
    model     := .claude .haiku5_5
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

## Supported models and providers

### Models

| Family | Constructor | Built-in variants |
| --- | --- | --- |
| Anthropic Claude | `Model.claude` | `.opus5_5`, `.sonnet5_5`, `.haiku5_5`, `.fable5_1`, `.opus5`, `.sonnet5`, `.opus4_5`, `.sonnet4_5`, `.haiku4_5`, `.custom id` |
| OpenAI | `Model.openai` | `.gpt6Astra`, `.gpt5_6Sol`, `.gpt5_6Terra`, `.gpt5_6Luna`, `.gpt5_5`, `.gpt5_4`, `.gpt4o`, `.gpt4oMini`, `.o3`, `.o4Mini`, `.custom id` |
| Google Gemini | `Model.gemini` | `.argon4`, `.flash3_8`, `.flash3_5`, `.pro2_5`, `.flash2_5`, `.custom id` |
| DeepSeek | `Model.deepseek` | `.v4_1Flash`, `.v4Pro`, `.v4Flash`, `.v3_2`, `.chat`, `.reasoner`, `.custom id` |
| Kimi / Moonshot | `Model.kimi` | `.k3`, `.k2_7Code`, `.k2_6`, `.k2_5`, `.k2Thinking`, `.forCoding`, `.custom id` |
| Z.AI GLM | `Model.glm` | `.glm5_3Prime`, `.glm5_3`, `.glm5_3Flash`, `.glm5_2`, `.glm5_1`, `.glm5`, `.glm4_7`, `.glm4_7Flash`, `.custom id` |
| Qwen | `Model.qwen` | `.qwen3_8Max`, `.qwen3_8Flash`, `.qwen3_7Max`, `.qwen3_7Plus`, `.qwen3_5Plus`, `.coderNext`, `.coderPlus`, `.custom id` |
| MiniMax | `Model.minimax` | `.m3`, `.m2_7`, `.m2_5`, `.m2`, `.custom id` |
| xAI Grok | `Model.grok` | `.grok4_7`, `.grok4_6`, `.grok4_5`, `.grok4_3`, `.grokBuild0_1`, `.codeFast1`, `.custom id` |
| Mistral | `Model.mistral` | `.large4`, `.medium3_5`, `.small2603`, `.devstral`, `.custom id` |

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
