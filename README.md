# leanllm

**A unified LLM client library for Lean 4, backed by [LeanHttp](https://github.com/theoriclabs/leanhttp).**

[![Lean](https://img.shields.io/badge/Lean-v4.34.1-555555)](https://lean-lang.org)

`leanllm` provides a single typed interface across Anthropic Claude, OpenAI, and Google Gemini models over direct vendor endpoints, OpenRouter, AWS Bedrock, Azure OpenAI, Google Vertex AI, and custom OpenAI/Anthropic-compatible servers.

`ModelConfig` separates the model family (`Model`), the hosting service (`Provider`), and the JSON wire dialect (`WireApi`). `WireApi.auto` selects the provider's default schema for a given model, while an explicit `WireApi` overrides the wire format per call (such as running OpenAI's `/v1/responses` or Anthropic's `/v1/messages` through OpenRouter). Conversations carry text, URL and base64 images, tool declarations (`ToolSpec`), tool calls (`ToolCall`), tool results (`ToolResult`), and reasoning traces (`ContentPart.thinking`). Requests run synchronously or stream incrementally over Server-Sent Events via `LeanHttp.Sse`, with transport faults, HTTP status codes, `Retry-After` headers, and schema mismatches mapped into a closed `LLMError` sum type.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the module dependency graph, request pipeline, and wire codec internals.

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

`LLMM.run` opens a `LeanHttp.Session`, executes the `LLMM` action, and closes the session on exit. For a single stateless call without `LLMM`, use `Leanllm.call` or `Leanllm.callStream`:

```lean
def oneShot : IO Unit := do
  let cfg : ModelConfig := { model := .gemini .flash35, maxTokens := some 128 }
  match ← (Leanllm.call cfg [Message.user "What is 2 + 2?"]).run with
  | .ok resp   => IO.println resp.text
  | .error err => IO.eprintln s!"Error: {repr err}"
```

## Configuration: `Model`, `Provider`, and `WireApi`

`ModelConfig` configures the target model, hosting provider, wire protocol, sampling parameters, and tool specifications:

```lean
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
```

### Models

| Family | Constructor | Built-in variants |
| --- | --- | --- |
| Anthropic Claude | `Model.claude` | `.opus55`, `.sonnet55`, `.haiku55`, `.fable51`, `.opus5`, `.sonnet5`, `.opus45`, `.sonnet45`, `.haiku45`, `.custom id` |
| OpenAI | `Model.openai` | `.gpt56Sol`, `.gpt54`, `.gpt4o`, `.gpt4oMini`, `.o3`, `.o4Mini`, `.custom id` |
| Google Gemini | `Model.gemini` | `.argon4`, `.flash38`, `.flash35`, `.pro25`, `.flash25`, `.custom id` |

### Providers and credentials

When `ModelConfig.apiKey` is `none`, `leanllm` reads the corresponding environment variable:

| Provider | Configuration | Environment variables | Default `WireApi` |
| --- | --- | --- | --- |
| `.direct` | Native vendor API | `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY` (or `GOOGLE_API_KEY`) | `.anthropicMessages` / `.openaiChat` / `.geminiGenerate` |
| `.openRouter cfg` | `OpenRouterConfig` (fallbacks, provider order, sort by `.price` / `.throughput` / `.latency`) | `OPENROUTER_API_KEY` | `.openaiChat` |
| `.bedrock cfg` | `BedrockConfig` (`region`, `crossRegion`) | `AWS_BEARER_TOKEN_BEDROCK`, `BEDROCK_API_KEY` | `.bedrockConverse` |
| `.azure cfg` | `AzureConfig` (`endpoint`, `deployment`, `apiVersion`) | `AZURE_OPENAI_API_KEY`, `OPENAI_API_KEY` | `.openaiChat` |
| `.vertex cfg` | `VertexConfig` (`projectId`, `location`) | `VERTEX_API_KEY`, `GOOGLE_ACCESS_TOKEN` | `.anthropicMessages` (Claude) / `.geminiGenerate` (Gemini) |
| `.custom baseUrl` | Custom base URL | `LLM_API_KEY`, `OPENAI_API_KEY` (optional) | `.openaiChat` |

### Switching providers and wire APIs in `LLMM`

Inside an `LLMM` block, `LLMM.withProvider`, `LLMM.withModel`, and `LLMM.withWireApi` override the context locally while reusing the same `LeanHttp.Session`:

```lean
import Leanllm

open Leanllm

def multiCall : LLMM (String × String) := do
  -- Uses the base config from LLMM.run
  let a ← ask "Give one word for fast."
  -- Switch to OpenRouter with the OpenAI Responses wire dialect for a sub-computation
  let b ← LLMM.withProvider (.openRouter { appName := some "my-app" }) <|
          LLMM.withModel (.openai .gpt4oMini) <|
          LLMM.withWireApi .openaiResponses <|
            ask "Give one word for slow."
  return (a, b)
```

## Messages, multimodal content, and tool calling

Conversations are lists of `Message` values. Each `Message` has a `Role` (`.system`, `.developer`, `.user`, `.assistant`, `.tool`) and a `List ContentPart`:

```lean
inductive ContentPart where
  | text       (text : String)
  | image      (source : ImageSource)
  | toolCall   (call : ToolCall)
  | toolResult (result : ToolResult)
  | thinking   (thought : String) (signature : Option String := none)
```

Helper constructors cover common cases: `Message.system`, `Message.developer`, `Message.user`, `Message.assistant`, and `Message.toolResult`.

### Tool declarations and tool calls

Define tools with `ToolSpec` and inspect `resp.toolCalls` on the returned `LLMResponse`:

```lean
import Leanllm

open Leanllm

def weatherTool : ToolSpec := {
  name        := "get_weather"
  description := "Get the current weather for a city"
  parameters  := Lean.Json.mkObj [
    ("type", "object"),
    ("properties", Lean.Json.mkObj [
      ("city", Lean.Json.mkObj [
        ("type", "string"),
        ("description", "City name, e.g. Tokyo")
      ])
    ]),
    ("required", Lean.toJson ["city"])
  ]
}

def runToolExample : IO Unit := do
  let cfg : ModelConfig := {
    model     := .claude .haiku55
    tools     := [weatherTool]
    maxTokens := some 256
  }
  let msgs := [Message.user "What is the weather in Tokyo?"]
  match ← LLMM.run cfg (completion msgs) with
  | .ok resp =>
    for tc in resp.toolCalls do
      IO.println s!"Tool call: {tc.name} with args {tc.arguments.compress} (id={tc.id})"
  | .error err =>
    IO.eprintln s!"Error: {repr err}"
```

`LLMResponse` also provides `resp.text : String`, `resp.thinking : List String`, `resp.stopReason : StopReason`, and `resp.usage : Option Usage` (with `inputTokens`, `outputTokens`, `cacheReadTokens`, `cacheWriteTokens`, and `reasoningTokens`).

## Streaming over Server-Sent Events

`completionStream` and `askStream` stream text deltas over SSE and return the full concatenated string when the stream finishes:

```lean
import Leanllm

open Leanllm

def streamExample : IO Unit := do
  let cfg : ModelConfig := { model := .claude .haiku55, maxTokens := some 128 }
  match ← LLMM.run cfg (askStream "Count from 1 to 5." fun delta => IO.print delta) with
  | .ok _      => IO.println ""
  | .error err => IO.eprintln s!"\nStream error: {repr err}"
```

SSE delta extraction supports `.anthropicMessages`, `.openaiChat`, `.openaiResponses`, `.geminiGenerate` (automatically rewritten to `:streamGenerateContent?alt=sse`), `.bedrockConverse`, and `.bedrockInvoke`.

## Error handling

Every operation fails with a typed `LLMError`:

| Constructor | Sub-kind type | Common triggers |
| --- | --- | --- |
| `.auth kind msg` | `AuthErrorKind` (`.missingApiKey`, `.invalidApiKey`, `.insufficientPermissions`) | Unset API key env var, HTTP `401`, HTTP `403` |
| `.rateLimit kind msg` | `RateLimitErrorKind` (`.requestsPerMinute (retryAfterMs : Option Nat)`, `.tokensPerMinute (retryAfterMs : Option Nat)`, `.quotaExceeded`) | HTTP `429` with parsed `Retry-After` header |
| `.invalidRequest kind msg` | `InvalidRequestKind` (`.contextLengthExceeded`, `.unsupportedFeature`, `.invalidToolSchema`, `.malformedParameters`) | Unsupported `(Provider, Model)` pair, HTTP `400` / `422`, invalid URL or header |
| `.network kind msg` | `NetworkErrorKind` (`.timeout`, `.dnsFailure`, `.connectionRefused`, `.tlsError`, `.transport`) | `LeanHttp` / libcurl transport failures |
| `.provider kind msg` | `ProviderErrorKind` (`.overloaded`, `.internalError`, `.badGateway`, `.statusCode code body`) | HTTP `500`, `502`, `503`, `529`, or other non-2xx responses |
| `.parse kind msg` | `ParseErrorKind` (`.invalidJson`, `.unexpectedSchema`, `.invalidToolArgs`) | Malformed JSON body, unexpected response structure, invalid tool argument JSON |
| `.contentBlocked reason` | `String` | Safety or content filter rejection |

## Building and running the test suite

Build the library and executable with Lake:

```bash
lake build
```

`Main.lean` runs 8 live integration checks covering direct Anthropic, direct Gemini, and OpenRouter across synchronous completions, tool calls, the OpenAI Responses wire API, and SSE streaming:

```bash
ANTHROPIC_API_KEY=... GEMINI_API_KEY=... OPENROUTER_API_KEY=... lake exe leanllm
```
