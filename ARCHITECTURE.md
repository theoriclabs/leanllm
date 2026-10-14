# Architecture

`leanllm` is a Lean 4 client library for large language model APIs, built on [`LeanHttp`](https://github.com/theoriclabs/leanhttp) (`v0.6.0`) and `Lean.Data.Json`. The library totals 1,680 lines of Lean across 12 library modules and one executable test runner (`Main.lean`).

## Design overview

Most multi-provider LLM clients conflate the model family, the hosting service, and the wire protocol. In practice these three vary independently:

1. `Leanllm.Model`: The model family and version, such as `.claude .sonnet55`, `.openai .gpt4oMini`, or `.gemini .flash35`.
2. `Leanllm.Provider`: Where the model is hosted and how requests are authenticated and routed (`.direct`, `.openRouter`, `.bedrock`, `.azure`, `.vertex`, or `.custom`).
3. `Leanllm.WireApi`: Which JSON schema goes over HTTP and Server-Sent Events (`.anthropicMessages`, `.openaiChat`, `.openaiResponses`, `.geminiGenerate`, `.bedrockConverse`, `.bedrockInvoke`, or `.auto`).

For example, Claude Haiku 5.5 can be called directly on Anthropic using `.anthropicMessages`, through OpenRouter using `.openaiChat` or `.anthropicMessages`, on AWS Bedrock using `.bedrockConverse` or `.bedrockInvoke`, or on Google Vertex AI using `.anthropicMessages`. `ModelConfig` holds all three axes and resolves `.auto` into a concrete wire dialect and model identifier before encoding or sending a request.

## Module layout and dependency graph

`Leanllm.lean` re-exports the library modules. Dependencies flow strictly bottom-up without cycles:

```text
Main
 └─► Leanllm.Basic (LLMM monad, call / callStream entry points)
      ├─► Leanllm.Transport (URL resolution, auth headers, LeanHttp JSON & SSE execution)
      ├─► Leanllm.Codec (bidirectional JSON & SSE wire codecs for all 6 WireApi dialects)
      └─► Leanllm.Response (LLMResponse, StopReason, Usage)
           └─► Leanllm.Models (ModelConfig, effectiveWireApi, effectiveModelId)
                ├─► Leanllm.Provider (Model, WireApi, Provider routing rules)
                │    ├─► Leanllm.Models.Claude
                │    ├─► Leanllm.Models.OpenAI
                │    └─► Leanllm.Models.Gemini
                ├─► Leanllm.Error (LLMError and 6 sub-kind sum types)
                └─► Leanllm.Message (Message, Role, ContentPart, ToolSpec, ToolCall, ToolResult)
```

| Module | File | LOC | Responsibility |
| --- | --- | ---: | --- |
| `Leanllm.Models.Claude` | `Leanllm/Models/Claude.lean` | 46 | `ClaudeModel` inductive type, native model IDs, and OpenRouter slugs |
| `Leanllm.Models.OpenAI` | `Leanllm/Models/OpenAI.lean` | 32 | `OpenAIModel` inductive type, native model IDs, and OpenRouter slugs |
| `Leanllm.Models.Gemini` | `Leanllm/Models/Gemini.lean` | 30 | `GeminiModel` inductive type, native model IDs, and OpenRouter slugs |
| `Leanllm.Provider` | `Leanllm/Provider.lean` | 128 | `Model`, `WireApi`, `Provider`, provider configs, compatibility checks, and slug resolution |
| `Leanllm.Message` | `Leanllm/Message.lean` | 71 | `Role`, `ContentPart`, `ImageSource`, `ToolSpec`, `ToolCall`, `ToolResult`, and `Message` constructors |
| `Leanllm.Error` | `Leanllm/Error.lean` | 56 | `LLMError` sum type and structured sub-error kinds |
| `Leanllm.Response` | `Leanllm/Response.lean` | 54 | `StopReason`, `Usage`, `LLMResponse`, and content extractors (`text`, `toolCalls`, `thinking`) |
| `Leanllm.Models` | `Leanllm/Models.lean` | 32 | `ModelConfig` record and `effectiveWireApi` / `effectiveModelId` helpers |
| `Leanllm.Codec` | `Leanllm/Codec.lean` | 732 | Request encoders, response decoders, streaming request adapters, and SSE delta parsers |
| `Leanllm.Transport` | `Leanllm/Transport.lean` | 246 | API key lookup, endpoint URL construction, HTTP/SSE execution, and HTTP error mapping |
| `Leanllm.Basic` | `Leanllm/Basic.lean` | 138 | `LLMContext`, `LLMM` monad, session management, and top-level completion functions |
| `Leanllm` | `Leanllm.lean` | 8 | Root umbrella module |
| `Main` | `Main.lean` | 107 | End-to-end integration test suite across live providers |

## Routing and resolution rules

`Leanllm.Provider` and `Leanllm.Models` resolve a `ModelConfig` into a concrete wire dialect and model string through three pure functions:

- `Provider.supportsModel : Provider → Model → Bool` checks whether the selected provider hosts the given model family.
- `ModelConfig.effectiveWireApi : ModelConfig → WireApi` keeps an explicit `cfg.wireApi` or calls `Provider.defaultWireApi cfg.provider cfg.model` when `cfg.wireApi == .auto`.
- `ModelConfig.effectiveModelId : ModelConfig → String` delegates to `Provider.resolveModelId cfg.provider cfg.model`.

| Provider | Supported model families | Default `WireApi` (`.auto`) | Model ID resolution (`resolveModelId`) |
| --- | --- | --- | --- |
| `.direct` | Claude, OpenAI, Gemini | `.anthropicMessages` (Claude), `.openaiChat` (OpenAI), `.geminiGenerate` (Gemini) | `m.toModelId` (e.g. `claude-sonnet-5-5`, `gpt-4o-mini`, `gemini-3.5-flash`) |
| `.openRouter cfg` | Claude, OpenAI, Gemini | `.openaiChat` | `m.toOpenRouterSlug` (e.g. `anthropic/claude-sonnet-5.5`, `openai/gpt-4o-mini`, `google/gemini-3.5-flash`) |
| `.bedrock cfg` | Claude only | `.bedrockConverse` | `[us.]anthropic.<toModelId>-v1:0` (`us.` prefix when `cfg.crossRegion == true`) |
| `.azure cfg` | OpenAI only | `.openaiChat` | `cfg.deployment.getD m.toModelId` |
| `.vertex cfg` | Claude, Gemini | `.anthropicMessages` (Claude), `.geminiGenerate` (Gemini) | `m.toModelId` |
| `.custom baseUrl` | Claude, OpenAI, Gemini | `.openaiChat` | `m.toModelId` |

## Request and streaming execution pipeline

Both synchronous calls (`callWithSession`) and streaming calls (`callStreamWithSession`) in `Leanllm/Basic.lean` run inside `ExceptT LLMError IO` and follow a five-stage pipeline:

```text
ModelConfig + List Message
  │
  ├─ 1. Provider.supportsModel      ──► Rejects unsupported (Provider, Model) pairs with .invalidRequest (.unsupportedFeature ...)
  ├─ 2. Transport.resolveApiKey     ──► Reads cfg.apiKey or environment variables; throws .auth .missingApiKey if absent
  ├─ 3. Codec.encode[Stream]Request ──► Encodes messages, system prompts, tools, and sampling options for effectiveWireApi
  ├─ 4. Transport.build[Stream]HttpRequest + sendJson / sendSse
  │                                 ──► Builds LeanHttp.Request (URL, auth headers, 15s connect / 120s total timeouts) and executes
  └─ 5. Codec.decodeResponse / decodeSseTextDelta
                                    ──► Normalizes provider JSON into LLMResponse or emits text chunks to onDelta
```

### Stage 1: Compatibility check

Before checking credentials or building JSON, `callWithSession` and `callStreamWithSession` verify `cfg.provider.supportsModel cfg.model`. An incompatible pair (such as a Gemini model on Azure or an OpenAI model on Bedrock) fails immediately with `LLMError.invalidRequest (.unsupportedFeature ...)`.

### Stage 2: Credential resolution

`Transport.resolveApiKey` returns `cfg.apiKey` when non-empty. Otherwise it checks environment variables in order for the `(Provider, Model)` pair:

- `.direct` + `.claude _`: `ANTHROPIC_API_KEY`
- `.direct` + `.openai _`: `OPENAI_API_KEY`
- `.direct` + `.gemini _`: `GEMINI_API_KEY`, then `GOOGLE_API_KEY`
- `.openRouter _`: `OPENROUTER_API_KEY`
- `.bedrock _`: `AWS_BEARER_TOKEN_BEDROCK`, then `BEDROCK_API_KEY`
- `.azure _`: `AZURE_OPENAI_API_KEY`, then `OPENAI_API_KEY`
- `.vertex _`: `VERTEX_API_KEY`, then `GOOGLE_ACCESS_TOKEN`
- `.custom _`: `LLM_API_KEY`, then `OPENAI_API_KEY` (returns `""` without throwing if neither is set, allowing unauthenticated local servers)

### Stage 3: Wire codec encoding

`Codec.encodeRequest` dispatches on `cfg.effectiveWireApi` to construct a `Lean.Json` request body from the canonical `List Message` and `ModelConfig`:

| Wire dialect | Encoder | System / developer messages | Multimodal & tool representation |
| --- | --- | --- | --- |
| `.anthropicMessages` | `encodeAnthropicMessages cfg msgs false` | Extracted and joined with `"\n\n"` into top-level `"system"` string | Content array of `text`, `image` (`url` or `base64`), `tool_use`, `tool_result`, and `thinking` (with optional `signature`). Defaults `max_tokens` to `4096`. |
| `.bedrockInvoke` | `encodeAnthropicMessages cfg msgs true` | Same as `.anthropicMessages` | Replaces `"model"` field with `"anthropic_version": "bedrock-2023-05-31"`. |
| `.openaiChat` | `encodeOpenAIChat cfg msgs` | Kept inline as messages with `"role": "system"` or `"developer"` | Expands `.toolResult` parts into separate `"role": "tool"` messages; encodes `.image` as `image_url` (converting base64 to `data:<mime>;base64,<data>`); serializes tool call arguments as compressed JSON strings; appends OpenRouter `models` and `provider` routing fields when `cfg.provider` is `.openRouter`. |
| `.openaiResponses` | `encodeOpenAIResponses cfg msgs` | Extracted into top-level `"instructions"` field | Encodes `"input"` array of typed items (`message`, `function_call`, `function_call_output`) and `"max_output_tokens"`. |
| `.geminiGenerate` | `encodeGeminiGenerate cfg msgs` | Extracted into `"systemInstruction"` object | Maps `.assistant` role to `"model"`; encodes parts as `text`, `inlineData` (base64), `fileData` (URL), `functionCall`, and `functionResponse`; places sampling parameters under `"generationConfig"` and tools under `"tools": [{"functionDeclarations": ...}]`. |
| `.bedrockConverse` | `encodeBedrockConverse cfg msgs` | Extracted into `"system": [{"text": ...}]` | Encodes content blocks as `text`, `toolUse`, and `toolResult` (with `"status": "error" | "success"`); places sampling parameters under `"inferenceConfig"` and tool schemas under `"toolConfig"`. |

For streaming requests, `Codec.encodeStreamRequest` calls `encodeRequest` and sets `"stream": true` on the root JSON object for `.anthropicMessages`, `.openaiChat`, `.openaiResponses`, and `.auto`. Gemini and Bedrock streaming endpoints do not use a body-level `"stream"` field.

### Stage 4: HTTP and SSE transport

`Transport.resolveEndpointUrl` selects the target URL:

- If `cfg.baseUrl` is `some base`, it strips any trailing slash and appends the dialect suffix (`/v1/messages`, `/chat/completions`, `/responses`, `/models/{id}:generateContent`, `/model/{id}/converse`, or `/model/{id}/invoke`).
- Otherwise it constructs the provider's canonical URL (for example `https://api.anthropic.com/v1/messages`, `https://openrouter.ai/api/v1/chat/completions`, `https://bedrock-runtime.{region}.amazonaws.com/model/{id}/converse`, `https://{endpoint}/openai/deployments/{deployment}/chat/completions`, or the regional Vertex AI `:rawPredict` / `:generateContent` URL).

`Transport.buildHttpRequest` creates a POST `LeanHttp.Request` with the JSON body, `Accept: application/json`, a 15-second connect timeout, and a 120-second total timeout, then attaches provider-specific authentication:

- Direct Anthropic: `x-api-key: <key>` and `anthropic-version: 2023-06-01`
- Direct Gemini: `x-goog-api-key: <key>`
- OpenRouter: `Authorization: Bearer <key>`, plus optional `HTTP-Referer` and `X-Title`
- Azure: `api-key: <key>` header and `api-version` query parameter
- Direct OpenAI, Bedrock, Vertex, and Custom (when key is non-empty): `Authorization: Bearer <key>`

For streaming (`Transport.buildStreamHttpRequest`), `.geminiGenerate` rewrites `:generateContent` to `:streamGenerateContent` and appends the query parameter `alt=sse`. `Transport.sendSse` sets `Accept: text/event-stream` and runs `LeanHttp.Session.requestSse` (or one-shot `LeanHttp.requestSse`).

### Stage 5: Response and SSE decoding

For synchronous calls, `Codec.decodeResponse` dispatches on `cfg.effectiveWireApi` to construct a normalized `LLMResponse`:

- Assistant text, tool calls (with JSON arguments parsed into `Lean.Json` values), and reasoning/thinking blocks (`thinking` in Anthropic, `reasoning_content` / `reasoning` in OpenAI Chat, `reasoning` summary items in OpenAI Responses, `thought: true` parts in Gemini, and `reasoningContent.reasoningText` in Bedrock Converse) are collected into `resp.message.content : List ContentPart`.
- Dialect-specific finish strings map to `StopReason` (`.endTurn`, `.maxTokens`, `.stopSequence`, `.toolUse`, `.contentFilter`, or `.other raw`).
- Input, output, cache read (`cache_read_input_tokens`, `cached_tokens`, `cachedContentTokenCount`, `cacheReadInputTokens`), cache write (`cache_creation_input_tokens`, `cacheWriteInputTokens`), and reasoning token counts (`reasoning_tokens`, `thoughtsTokenCount`) populate `Usage`.

For streaming calls, `Codec.decodeSseTextDelta` inspects each `LeanHttp.Sse.Event`:

- Ignores empty payloads and `[DONE]` sentinels.
- `.anthropicMessages` / `.bedrockInvoke`: extracts `delta.text` or `delta.thinking` when `type == "content_block_delta"`.
- `.openaiChat` / `.auto`: extracts `choices[0].delta.content`.
- `.openaiResponses`: extracts `delta` when `type == "response.output_text.delta"`.
- `.geminiGenerate`: concatenates `text` fields from `candidates[0].content.parts` where `thought` is not `true`.
- `.bedrockConverse`: extracts `delta.text`.

Each non-empty delta is appended to an `IO.Ref String` accumulator and passed to the caller's `onDelta : String → IO Unit` callback.

## Error translation

All failures surface as `LLMError` (`Leanllm/Error.lean`):

- `Transport.fromHttpError` maps `LeanHttp.Error.kind` values to `LLMError.network` (`.timeout`, `.dnsFailure`, `.connectionRefused`, `.tlsError`, `.transport`) or `LLMError.invalidRequest (.malformedParameters ...)` for malformed URLs and unsupported protocols.
- `Transport.fromHttpStatus` parses JSON error bodies (`error.message`, `error.code`, `error.type`, or top-level `message`) and maps status codes:
  - `401` → `.auth .invalidApiKey`
  - `403` → `.auth .insufficientPermissions`
  - `400`, `422` → `.invalidRequest (.contextLengthExceeded none)` when the error code indicates context overflow, otherwise `.invalidRequest (.malformedParameters msg)`
  - `429` → `.rateLimit`, inspecting the `retry-after` response header (converted to milliseconds) and distinguishing `.quotaExceeded`, `.tokensPerMinute`, and `.requestsPerMinute`
  - `500` → `.provider .internalError`
  - `502` → `.provider .badGateway`
  - `503`, `529` → `.provider .overloaded`
  - Other non-2xx statuses → `.provider (.statusCode code rawBody)`
- `Codec` and `Transport` return `LLMError.parse` (`.invalidJson`, `.unexpectedSchema`, or `.invalidToolArgs`) when a response body is malformed JSON, omits required schema fields, or contains unparseable tool call arguments.

## The `LLMM` effect monad and session lifecycle

`Leanllm/Basic.lean` defines:

```lean
structure LLMContext where
  config  : ModelConfig
  session : Option LeanHttp.Session := none

abbrev LLMM (α : Type) := ReaderT LLMContext (ExceptT LLMError IO) α
```

`LLMM.run` allocates a single `LeanHttp.Session` (backed by a libcurl easy handle in `LeanHttp`), runs the `LLMM` computation with connection reuse across all requests in the block, and closes the session in both normal and exception paths. Callers that manage their own session lifecycle can use `LLMM.runWithSession` or pass `some session` directly to `callWithSession` and `callStreamWithSession`.

Within an `LLMM` block, `LLMM.withConfig`, `LLMM.withProvider`, `LLMM.withModel`, and `LLMM.withWireApi` use `ReaderT.withReader` to override configuration locally for sub-computations while sharing the underlying `LeanHttp.Session`.
