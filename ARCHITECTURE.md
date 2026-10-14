# Architecture

`leanllm` is a Lean 4 client library for large language model APIs, built on [`LeanHttp`](https://github.com/theoriclabs/leanhttp) (`v0.6.0`) and `Lean.Data.Json`. The library totals 2,944 lines of Lean across 16 library modules and one executable test runner (`Main.lean`).

## Design overview

Most multi-provider LLM clients conflate the model family, the hosting service, the wire protocol, and the authentication mode (API key vs. subscription OAuth). In practice these four axes vary independently:

1. `Leanllm.Model`: The model family and version — `.claude`, `.openai`, `.gemini`, `.deepseek`, `.kimi`, `.glm`, `.qwen`, `.minimax`, `.grok`, or `.mistral`.
2. `Leanllm.Provider`: Where the model is hosted and how requests are routed (`.direct`, `.openRouter`, `.bedrock`, `.azure`, `.vertex`, `.copilot`, `.antigravity`, `.grokBuild`, `.kimiCode`, `.zaiCoding`, `.alibabaCoding`, `.opencodeZen`, `.opencodeGo`, `.groq`, `.cerebras`, `.together`, `.fireworks`, `.deepinfra`, `.novita`, `.baseten`, `.nebius`, `.nvidiaNim`, `.ollama`, `.lmstudio`, or `.custom`).
3. `Leanllm.WireApi`: Which JSON schema goes over HTTP and Server-Sent Events (`.anthropicMessages`, `.openaiChat`, `.openaiResponses`, `.geminiGenerate`, `.bedrockConverse`, `.bedrockInvoke`, or `.auto`).
4. `Leanllm.AuthMode`: Whether to authenticate with a per-token API key or reuse a local subscription OAuth session (`.auto`, `.oauth account?`, `.apiKey key?`, or `.bearer token`).

## Module layout and dependency graph

`Leanllm.lean` re-exports the library modules. Dependencies flow strictly bottom-up without cycles:

```text
Main
 └─► Leanllm.Basic (LLMM monad, call / callStream entry points)
      ├─► Leanllm.Transport (Auth resolution, URL resolution, LeanHttp JSON & SSE execution)
      │    ├─► Leanllm.Auth.OpenAI (Codex / ChatGPT subscription OAuth discovery & refresh)
      │    ├─► Leanllm.Auth.Claude (Claude Code / Keychain / jcode OAuth discovery & refresh)
      │    └─► Leanllm.Auth.Other  (Gemini Code Assist, Antigravity, Copilot, Grok Build OAuth)
      │         └─► Leanllm.Auth   (AuthMode, OAuthCredentials, ResolvedAuth, JWT / Base64URL)
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
| `Leanllm.Models.OpenAI` | `Leanllm/Models/OpenAI.lean` | 40 | `OpenAIModel` inductive type, native model IDs, and OpenRouter slugs |
| `Leanllm.Models.Gemini` | `Leanllm/Models/Gemini.lean` | 30 | `GeminiModel` inductive type, native model IDs, and OpenRouter slugs |
| `Leanllm.Models.DeepSeek` | `Leanllm/Models/DeepSeek.lean` | 33 | `DeepSeekModel` inductive type (`v4_1Flash`, `v4Pro`, `v4Flash`, `v3_2`, `chat`, `reasoner`) |
| `Leanllm.Models.Kimi` | `Leanllm/Models/Kimi.lean` | 34 | `KimiModel` inductive type (`k3`, `k2_7Code`, `k2_6`, `k2_5`, `k2Thinking`, `forCoding`) |
| `Leanllm.Models.Glm` | `Leanllm/Models/Glm.lean` | 37 | `GlmModel` inductive type (`glm5_3Prime`, `glm5_3`, `glm5_3Flash`, `glm5_2`, `glm5_1`, `glm5`, `glm4_7`, `glm4_7Flash`) |
| `Leanllm.Models.Qwen` | `Leanllm/Models/Qwen.lean` | 36 | `QwenModel` inductive type (`qwen3_8Max`, `qwen3_8Flash`, `qwen3_7Max`, `qwen3_7Plus`, `qwen3_5Plus`, `coderNext`, `coderPlus`) |
| `Leanllm.Models.MiniMax` | `Leanllm/Models/MiniMax.lean` | 29 | `MiniMaxModel` inductive type (`m3`, `m2_7`, `m2_5`, `m2`) |
| `Leanllm.Models.Grok` | `Leanllm/Models/Grok.lean` | 33 | `GrokModel` inductive type (`grok4_7`, `grok4_6`, `grok4_5`, `grok4_3`, `grokBuild0_1`, `codeFast1`) |
| `Leanllm.Models.Mistral` | `Leanllm/Models/Mistral.lean` | 29 | `MistralModel` inductive type (`large4`, `medium3_5`, `small2603`, `devstral`) |
| `Leanllm.Provider` | `Leanllm/Provider.lean` | 144 | `Model`, `WireApi`, `Provider`, provider configs, compatibility checks, and slug resolution |
| `Leanllm.Message` | `Leanllm/Message.lean` | 71 | `Role`, `ContentPart`, `ImageSource`, `ToolSpec`, `ToolCall`, `ToolResult`, and `Message` constructors |
| `Leanllm.Error` | `Leanllm/Error.lean` | 56 | `LLMError` sum type and structured sub-error kinds |
| `Leanllm.Response` | `Leanllm/Response.lean` | 54 | `StopReason`, `Usage`, `LLMResponse`, and content extractors (`text`, `toolCalls`, `thinking`) |
| `Leanllm.Auth` | `Leanllm/Auth.lean` | 155 | `AuthMode`, `CredentialSource`, `OAuthCredentials`, `ResolvedAuth`, Base64URL/JWT claim decoding |
| `Leanllm.Auth.OpenAI` | `Leanllm/Auth/OpenAI.lean` | 184 | OpenAI ChatGPT/Codex OAuth loading (`~/.codex/auth.json`, `~/.jcode/openai-auth.json`), JWT `chatgpt_account_id` extraction, token refresh & persistence |
| `Leanllm.Auth.Claude` | `Leanllm/Auth/Claude.lean` | 230 | Claude Pro/Max OAuth loading (`~/.jcode/auth.json`, `~/.claude/.credentials.json`, macOS Keychain, `$CLAUDE_CODE_OAUTH_TOKEN`), token refresh, and tool name mapping |
| `Leanllm.Auth.Other` | `Leanllm/Auth/Other.lean` | 291 | Gemini Code Assist, Antigravity, GitHub Copilot token exchange, and xAI Grok Build OIDC |
| `Leanllm.Models` | `Leanllm/Models.lean` | 44 | `ModelConfig` record and `effectiveWireApi` / `effectiveWireApiForAuth` / `effectiveModelId` helpers |
| `Leanllm.Codec` | `Leanllm/Codec.lean` | 838 | Request encoders, response decoders, OAuth payload shaping, and SSE delta parsers |
| `Leanllm.Transport` | `Leanllm/Transport.lean` | 421 | `resolveAuth`, endpoint URL construction, HTTP/SSE execution, and HTTP error mapping |
| `Leanllm.Basic` | `Leanllm/Basic.lean` | 219 | `LLMContext`, `LLMM` monad (`withProvider`, `withWireApi`, `withModel`, `withAuth`), and completion functions |
| `Leanllm` | `Leanllm.lean` | 12 | Root umbrella module |
| `Main` | `Main.lean` | 109 | End-to-end integration test suite across live subscription OAuth and API-key providers |

## Subscription OAuth Reuse and Refresh

`Transport.resolveAuth` resolves `cfg.auth` (`AuthMode`) into a concrete `ResolvedAuth`:

1. **OpenAI ChatGPT / Codex Subscription (`Leanllm.Auth.OpenAI`)**:
   - Loads tokens from `~/.jcode/openai-auth.json`, `~/.codex/auth.json`, or `~/.local/share/opencode/auth.json`.
   - Base64URL-decodes the JWT payload (`Auth.decodeJwtPayload?`) to recover `exp` (expiry in milliseconds), `client_id` / `aud`, and `https://api.openai.com/auth.chatgpt_account_id`.
   - Proactively refreshes tokens expiring within 5 minutes via `POST https://auth.openai.com/oauth/token` (using the `client_id` extracted from the JWT or `LEANLLM_OPENAI_CLIENT_ID`) and atomically persists rotated tokens back to disk.
   - Routes requests to `https://chatgpt.com/backend-api/codex/responses` using `WireApi.openaiResponses` (`store: false`, `stream: true`) with `Authorization: Bearer <token>`, `originator: codex_cli_rs`, and `chatgpt-account-id: <accountId>`.
2. **Claude Pro / Max Subscription (`Leanllm.Auth.Claude`)**:
   - Loads tokens from `$CLAUDE_CODE_OAUTH_TOKEN`, `~/.jcode/auth.json` (`anthropic_accounts`), `~/.claude/.credentials.json`, macOS Keychain (`Claude Code-credentials`), or `~/.local/share/opencode/auth.json`.
   - Refreshes expiring tokens via `POST https://platform.claude.com/v1/oauth/token` (using `client_id` from the credential store or `LEANLLM_CLAUDE_CLIENT_ID`, with fallback for legacy scopes) and persists rotated tokens back to disk.
   - Routes requests to `https://api.anthropic.com/v1/messages?beta=true` with `Authorization: Bearer <token>`, `User-Agent: claude-cli/2.1.280 (external, sdk-cli)`, `anthropic-beta` OAuth headers, `x-app: cli`, Claude Code billing/identity system blocks, and reversible PascalCase tool name mapping (`bash` ↔ `Bash`, `read` ↔ `Read`, etc.).
3. **Other Subscription Providers (`Leanllm.Auth.Other`)**:
   - **Gemini Code Assist & Antigravity**: Loads from `~/.jcode/gemini_oauth.json`, `~/.gemini/oauth_creds.json`, or `~/.jcode/antigravity_oauth.json` (extracting `client_id` from the `id_token` JWT `aud`/`azp` claim or env), resolves `cloudaicompanionProject` via `v1internal:loadCodeAssist`, and wraps requests for `cloudcode-pa.googleapis.com/v1internal:generateContent`.
   - **GitHub Copilot (`.copilot`)**: Reads `~/.config/github-copilot/hosts.json` or `apps.json`, exchanges the GitHub OAuth token at `https://api.github.com/copilot_internal/v2/token` for a short-lived Copilot bearer token, and calls `https://api.githubcopilot.com/chat/completions`.
   - **xAI Grok Build (`.grokBuild`)**: Reads OIDC credentials and `oidc_client_id` dynamically from `~/.grok/auth.json`, refreshes via `https://auth.x.ai/oauth2/token`, and calls `https://cli-chat-proxy.grok.com/v1/chat/completions` with Grok CLI headers.
