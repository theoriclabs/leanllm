import LeanHttp
import Lean.Data.Json
import Leanllm.Provider
import Leanllm.Models
import Leanllm.Error
import Leanllm.Auth
import Leanllm.Auth.OpenAI
import Leanllm.Auth.Claude
import Leanllm.Auth.Other

namespace Leanllm.Transport

open Lean LeanHttp Std.Http

/-- Default environment variable names and jcode `.env` file names for API keys per `(Provider, Model)`. -/
def apiKeyEnvVarsAndFile (provider : Provider) (model : Model) : (List String × Option String) :=
  match provider, model with
  | .direct, .claude _          => (["ANTHROPIC_API_KEY"], some "anthropic.env")
  | .direct, .openai _          => (["OPENAI_API_KEY"], some "openai.env")
  | .direct, .gemini _          => (["GEMINI_API_KEY", "GOOGLE_API_KEY"], some "gemini.env")
  | .direct, .deepseek _        => (["DEEPSEEK_API_KEY"], some "deepseek.env")
  | .direct, .kimi .forCoding   => (["KIMI_API_KEY", "MOONSHOT_API_KEY"], some "kimi.env")
  | .direct, .kimi _            => (["MOONSHOT_API_KEY", "KIMI_API_KEY"], some "moonshotai.env")
  | .direct, .glm _             => (["ZHIPU_API_KEY", "ZAI_API_KEY"], some "zai.env")
  | .direct, .qwen _            => (["BAILIAN_CODING_PLAN_API_KEY", "DASHSCOPE_API_KEY"], some "alibaba-coding-plan.env")
  | .direct, .minimax _         => (["MINIMAX_API_KEY"], some "minimax.env")
  | .direct, .grok _            => (["XAI_API_KEY", "GROK_DEPLOYMENT_KEY"], some "xai.env")
  | .direct, .mistral _         => (["MISTRAL_API_KEY"], some "mistral.env")
  | .openRouter _, _            => (["OPENROUTER_API_KEY"], some "openrouter.env")
  | .bedrock _, _               => (["AWS_BEARER_TOKEN_BEDROCK", "BEDROCK_API_KEY"], some "bedrock.env")
  | .azure _, _                 => (["AZURE_OPENAI_API_KEY", "OPENAI_API_KEY"], some "azure.env")
  | .vertex _, _                => (["VERTEX_API_KEY", "GOOGLE_ACCESS_TOKEN"], none)
  | .copilot, _                 => (["GITHUB_COPILOT_TOKEN"], none)
  | .antigravity, _             => (["ANTIGRAVITY_API_KEY"], none)
  | .grokBuild, _               => (["GROK_DEPLOYMENT_KEY", "XAI_API_KEY"], some "xai.env")
  | .kimiCode, _                => (["KIMI_API_KEY", "MOONSHOT_API_KEY"], some "kimi.env")
  | .zaiCoding, _               => (["ZHIPU_API_KEY", "ZAI_API_KEY"], some "zai.env")
  | .alibabaCoding, _           => (["BAILIAN_CODING_PLAN_API_KEY", "DASHSCOPE_API_KEY"], some "alibaba-coding-plan.env")
  | .opencodeZen, _             => (["OPENCODE_API_KEY"], some "opencode.env")
  | .opencodeGo, _              => (["OPENCODE_GO_API_KEY", "OPENCODE_API_KEY"], some "opencode-go.env")
  | .groq, _                    => (["GROQ_API_KEY"], some "groq.env")
  | .cerebras, _                => (["CEREBRAS_API_KEY"], some "cerebras.env")
  | .together, _                => (["TOGETHER_API_KEY"], some "togetherai.env")
  | .fireworks, _               => (["FIREWORKS_API_KEY"], some "fireworks.env")
  | .deepinfra, _               => (["DEEPINFRA_API_KEY"], some "deepinfra.env")
  | .novita, _                  => (["NOVITA_API_KEY"], some "novita.env")
  | .baseten, _                 => (["BASETEN_API_KEY"], some "baseten.env")
  | .nebius, _                  => (["NEBIUS_API_KEY"], some "nebius.env")
  | .nvidiaNim, _               => (["NVIDIA_API_KEY"], some "nvidia-nim.env")
  | .ollama, _                  => (["OLLAMA_API_KEY"], some "ollama.env")
  | .lmstudio, _                => (["LMSTUDIO_API_KEY"], some "lmstudio.env")
  | .custom _, _                => (["LLM_API_KEY", "OPENAI_API_KEY"], some "openai-compatible.env")

/-- Default environment variable names for API keys per `(Provider, Model)`. -/
def apiKeyEnvVars (provider : Provider) (model : Model) : List String :=
  (apiKeyEnvVarsAndFile provider model).1

/-- Try to find a configured API key in `cfg.apiKey`, process environment variables,
    saved `.env` files (`~/.config/jcode/*.env`), or external agent stores (`opencode`, `pi`, `hermes`). -/
def findConfiguredApiKey? (cfg : ModelConfig) : IO (Option String) := do
  if let some key := cfg.apiKey then
    if !key.isEmpty then return some key
  let (envVars, envFile?) := apiKeyEnvVarsAndFile cfg.provider cfg.model
  for var in envVars do
    if let some val ← IO.getEnv var then
      if !val.trimAscii.toString.isEmpty then
        return some val.trimAscii.toString
  if let some envFile := envFile? then
    for var in envVars do
      if let some val ← Auth.Other.loadFromJcodeEnvFile? var envFile then
        return some val
  for var in envVars do
    if let some val ← Auth.Other.loadFromExternalAgentStores? var then
      return some val
  return none

/-- Resolve the API key from `cfg.apiKey`, environment, or local credential stores. -/
def resolveApiKey (cfg : ModelConfig) : ExceptT LLMError IO String := do
  if let some key ← liftM (findConfiguredApiKey? cfg) then
    return key
  match cfg.provider with
  | .custom _ | .ollama | .lmstudio => return ""
  | _ =>
    let varsStr := String.intercalate " or " (apiKeyEnvVars cfg.provider cfg.model)
    throw (.auth .missingApiKey s!"Missing API key: set ModelConfig.apiKey or environment variable {varsStr}")

/-- Resolve the concrete `ResolvedAuth` for a `ModelConfig` (API key, subscription OAuth, or bearer token). -/
def resolveAuth (cfg : ModelConfig) (session : Option Session := none) :
    ExceptT LLMError IO ResolvedAuth := do
  match cfg.provider with
  | .copilot =>
    let tok ← Auth.Other.exchangeCopilotBearerToken session
    return .copilotBearer tok
  | .antigravity =>
    let (creds, proj) ← Auth.Other.loadAndEnsureFreshAntigravity session
    return .antigravityOAuth creds proj
  | .grokBuild =>
    let creds ← Auth.Other.loadAndEnsureFreshGrok session
    return .grokOAuth creds
  | _ =>
    match cfg.auth with
    | .bearer tok => return .bearer tok
    | .apiKey (some k) => return .apiKey k
    | .apiKey none => return .apiKey (← resolveApiKey cfg)
    | .oauth requestedAccount =>
      match cfg.provider, cfg.model with
      | .direct, .openai _ =>
        let creds ← Auth.OpenAI.loadAndEnsureFresh requestedAccount session
        return .openaiOAuth creds
      | .direct, .claude _ =>
        let creds ← Auth.Claude.loadAndEnsureFresh requestedAccount session
        return .claudeOAuth creds
      | .direct, .gemini _ =>
        let (creds, proj) ← Auth.Other.loadAndEnsureFreshGemini session
        return .geminiOAuth creds proj
      | .direct, .grok _ =>
        let creds ← Auth.Other.loadAndEnsureFreshGrok session
        return .grokOAuth creds
      | _, _ =>
        throw (.invalidRequest
          (.unsupportedFeature s!"Subscription OAuth is not supported for provider {repr cfg.provider} model {cfg.model}")
          "Unsupported OAuth provider")
    | .auto =>
      if let some key ← liftM (findConfiguredApiKey? cfg) then
        return .apiKey key
      -- If no API key is configured, automatically try local subscription OAuth for `.direct`
      match cfg.provider, cfg.model with
      | .direct, .openai _ =>
        let creds ← Auth.OpenAI.loadAndEnsureFresh none session
        return .openaiOAuth creds
      | .direct, .claude _ =>
        let creds ← Auth.Claude.loadAndEnsureFresh none session
        return .claudeOAuth creds
      | .direct, .gemini _ =>
        let (creds, proj) ← Auth.Other.loadAndEnsureFreshGemini session
        return .geminiOAuth creds proj
      | .direct, .grok _ =>
        let creds ← Auth.Other.loadAndEnsureFreshGrok session
        return .grokOAuth creds
      | _, _ =>
        return .apiKey (← resolveApiKey cfg)

/-- Map a `LeanHttp.Error` transport failure to a typed `LLMError`. -/
def fromHttpError (err : LeanHttp.Error) : LLMError :=
  let msg := toString err
  match err.kind with
  | .timeout =>
    .network .timeout msg
  | .couldntResolveHost | .couldntResolveProxy =>
    .network .dnsFailure msg
  | .couldntConnect =>
    .network .connectionRefused msg
  | .ssl _ =>
    .network .tlsError msg
  | .urlMalformed | .unsupportedProtocol =>
    .invalidRequest (.malformedParameters err.detail) msg
  | _ =>
    .network (.transport err.detail) msg

private def extractErrorBodyMessage (rawBody : String) : (String × Option String) :=
  match Json.parse rawBody with
  | .error _ => (rawBody, none)
  | .ok j =>
    if let .ok errObj := j.getObjVal? "error" then
      let msg := (errObj.getObjValAs? String "message").toOption
        |>.or (errObj.getStr?.toOption)
        |>.getD rawBody
      let code := (errObj.getObjValAs? String "code").toOption
        |>.or ((errObj.getObjValAs? String "type").toOption)
      (msg, code)
    else if let .ok msg := j.getObjValAs? String "message" then
      (msg, none)
    else
      (rawBody, none)

private def parseRetryAfterMs (resp : LeanHttp.Response) : Option Nat := do
  let val ← resp.header? headerName!"retry-after"
  let secs ← val.trimAscii.toString.toNat?
  some (secs * 1000)

/-- Map a non-2xx `LeanHttp.Response` to a typed `LLMError`. -/
def fromHttpStatus (resp : LeanHttp.Response) : LLMError :=
  let code := resp.statusCode.toNat
  let rawBody := (String.fromUTF8? resp.body).getD "<non-UTF8 error body>"
  let (msg, errCode) := extractErrorBodyMessage rawBody
  match code with
  | 401 => .auth .invalidApiKey msg
  | 403 => .auth .insufficientPermissions msg
  | 429 =>
    let retryMs := parseRetryAfterMs resp
    let isQuota := errCode.any fun c =>
      c == "insufficient_quota" || c == "quota_exceeded"
    let isTpm := errCode.any fun c =>
      c == "tokens" || c == "rate_limit_exceeded_tokens"
    let kind :=
      if isQuota then .quotaExceeded
      else if isTpm then .tokensPerMinute retryMs
      else .requestsPerMinute retryMs
    .rateLimit kind msg
  | 400 | 422 =>
    let isCtx := errCode.any fun c =>
      c == "context_length_exceeded" || c == "string_above_max_length"
    let kind :=
      if isCtx then .contextLengthExceeded none
      else .malformedParameters msg
    .invalidRequest kind msg
  | 500 => .provider .internalError msg
  | 502 => .provider .badGateway msg
  | 503 | 529 => .provider .overloaded msg
  | _ => .provider (.statusCode code rawBody) msg

private def setHeaderOrThrow (req : Request) (name : Header.Name) (rawValue : String) :
    Except LLMError Request := do
  let some next := req.header? name rawValue
    | throw (.invalidRequest (.malformedParameters s!"Invalid HTTP header value for {name}") "Invalid header value")
  return next

private def parseTarget (url : String) : Except LLMError Target := do
  let some uri := URI.parse? url
    | throw (.invalidRequest (.malformedParameters s!"Invalid endpoint URL: {url}") "Invalid endpoint URL")
  return .absolute uri

private def appendOpenAISuffix (base : String) (api : WireApi) : String :=
  let trimmed := if base.endsWith "/" then (base.dropEnd 1).toString else base
  match api with
  | .anthropicMessages => s!"{trimmed}/v1/messages"
  | .openaiResponses   => s!"{trimmed}/responses"
  | _                  => s!"{trimmed}/chat/completions"

/-- Construct the full endpoint URL for a `ModelConfig`. -/
def resolveEndpointUrl (cfg : ModelConfig) : String :=
  let api := cfg.effectiveWireApi
  let modelId := cfg.effectiveModelId
  if let some base := cfg.baseUrl then
    let trimmed := if base.endsWith "/" then (base.dropEnd 1).toString else base
    match api with
    | .anthropicMessages => s!"{trimmed}/v1/messages"
    | .openaiChat        => s!"{trimmed}/chat/completions"
    | .openaiResponses   => s!"{trimmed}/responses"
    | .geminiGenerate    => s!"{trimmed}/models/{modelId}:generateContent"
    | .bedrockConverse   => s!"{trimmed}/model/{modelId}/converse"
    | .bedrockInvoke     => s!"{trimmed}/model/{modelId}/invoke"
    | .auto              => s!"{trimmed}/chat/completions"
  else
    match cfg.provider with
    | .direct =>
      match cfg.model, api with
      | _,                  .anthropicMessages => "https://api.anthropic.com/v1/messages"
      | _,                  .geminiGenerate    => s!"https://generativelanguage.googleapis.com/v1beta/models/{modelId}:generateContent"
      | _,                  .bedrockConverse   => s!"https://bedrock-runtime.us-east-1.amazonaws.com/model/{modelId}/converse"
      | _,                  .bedrockInvoke     => s!"https://bedrock-runtime.us-east-1.amazonaws.com/model/{modelId}/invoke"
      | .deepseek _,        _                  => appendOpenAISuffix "https://api.deepseek.com" api
      | .kimi .forCoding,   _                  => appendOpenAISuffix "https://api.kimi.com/coding/v1" api
      | .kimi _,            _                  => appendOpenAISuffix "https://api.moonshot.ai/v1" api
      | .glm _,             _                  => appendOpenAISuffix "https://api.z.ai/api/coding/paas/v4" api
      | .qwen _,            _                  => appendOpenAISuffix "https://coding-intl.dashscope.aliyuncs.com/v1" api
      | .minimax _,         _                  => appendOpenAISuffix "https://api.minimax.io/v1" api
      | .grok _,            _                  => appendOpenAISuffix "https://api.x.ai/v1" api
      | .mistral _,         _                  => appendOpenAISuffix "https://api.mistral.ai/v1" api
      | _,                  .openaiResponses   => "https://api.openai.com/v1/responses"
      | _,                  _                  => "https://api.openai.com/v1/chat/completions"
    | .openRouter _ =>
      match api with
      | .openaiResponses   => "https://openrouter.ai/api/v1/responses"
      | .anthropicMessages => "https://openrouter.ai/api/v1/messages"
      | _                  => "https://openrouter.ai/api/v1/chat/completions"
    | .bedrock brCfg =>
      let action := match api with
        | .bedrockInvoke => "invoke"
        | _              => "converse"
      s!"https://bedrock-runtime.{brCfg.region}.amazonaws.com/model/{modelId}/{action}"
    | .azure azCfg =>
      let trimmed := if azCfg.endpoint.endsWith "/" then (azCfg.endpoint.dropEnd 1).toString else azCfg.endpoint
      let deployment := azCfg.deployment.getD modelId
      match api with
      | .openaiResponses => s!"{trimmed}/openai/v1/responses"
      | _                => s!"{trimmed}/openai/deployments/{deployment}/chat/completions"
    | .vertex vxCfg =>
      let host := s!"https://{vxCfg.location}-aiplatform.googleapis.com/v1/projects/{vxCfg.projectId}/locations/{vxCfg.location}"
      match cfg.model with
      | .claude _ => s!"{host}/publishers/anthropic/models/{modelId}:rawPredict"
      | .gemini _ => s!"{host}/publishers/google/models/{modelId}:generateContent"
      | _         => s!"{host}/endpoints/openapi/chat/completions"
    | .copilot =>
      match api with
      | .openaiResponses => "https://api.githubcopilot.com/responses"
      | _                => "https://api.githubcopilot.com/chat/completions"
    | .antigravity   => s!"{Auth.Other.antigravityEndpoint}:generateContent"
    | .grokBuild     => appendOpenAISuffix "https://cli-chat-proxy.grok.com/v1" api
    | .kimiCode      => appendOpenAISuffix "https://api.kimi.com/coding/v1" api
    | .zaiCoding     => appendOpenAISuffix "https://api.z.ai/api/coding/paas/v4" api
    | .alibabaCoding => appendOpenAISuffix "https://coding-intl.dashscope.aliyuncs.com/v1" api
    | .opencodeZen   => appendOpenAISuffix "https://opencode.ai/zen/v1" api
    | .opencodeGo    => appendOpenAISuffix "https://opencode.ai/zen/go/v1" api
    | .groq          => appendOpenAISuffix "https://api.groq.com/openai/v1" api
    | .cerebras      => appendOpenAISuffix "https://api.cerebras.ai/v1" api
    | .together      => appendOpenAISuffix "https://api.together.xyz/v1" api
    | .fireworks     => appendOpenAISuffix "https://api.fireworks.ai/inference/v1" api
    | .deepinfra     => appendOpenAISuffix "https://api.deepinfra.com/v1/openai" api
    | .novita        => appendOpenAISuffix "https://api.novita.ai/openai" api
    | .baseten       => appendOpenAISuffix "https://inference.baseten.co/v1" api
    | .nebius        => appendOpenAISuffix "https://api.tokenfactory.nebius.com/v1" api
    | .nvidiaNim     => appendOpenAISuffix "https://integrate.api.nvidia.com/v1" api
    | .ollama        => appendOpenAISuffix "http://localhost:11434/v1" api
    | .lmstudio      => appendOpenAISuffix "http://localhost:1234/v1" api
    | .custom base   => appendOpenAISuffix base api

/-- Construct the endpoint URL taking into account the resolved `ResolvedAuth` (including key-prefix routing for MiniMax & Kimi). -/
def resolveEndpointUrlWithAuth (cfg : ModelConfig) (auth : ResolvedAuth) : String :=
  if cfg.baseUrl.isSome then
    resolveEndpointUrl cfg
  else
    match auth with
    | .openaiOAuth _        => Auth.OpenAI.chatGptResponsesUrl
    | .claudeOAuth _        => Auth.Claude.oauthMessagesUrl
    | .geminiOAuth _ _      => s!"{Auth.Other.codeAssistEndpoint}:generateContent"
    | .antigravityOAuth _ _ => s!"{Auth.Other.antigravityEndpoint}:generateContent"
    | .copilotBearer _      => resolveEndpointUrl { cfg with provider := .copilot }
    | .grokOAuth _          => "https://cli-chat-proxy.grok.com/v1/chat/completions"
    | .apiKey key =>
      match cfg.provider, cfg.model with
      | .direct, .minimax _ =>
        -- MiniMax China Coding Plan keys start with `sk-cp-` and route to `api.minimaxi.com`
        if key.trimAscii.toString.startsWith "sk-cp-" then
          appendOpenAISuffix "https://api.minimaxi.com/v1" cfg.effectiveWireApi
        else
          resolveEndpointUrl cfg
      | .direct, .kimi _ =>
        -- Kimi Code keys (`sk-kimi-...`) target `api.kimi.com/coding/v1`
        if key.trimAscii.toString.startsWith "sk-kimi-" then
          appendOpenAISuffix "https://api.kimi.com/coding/v1" cfg.effectiveWireApi
        else
          resolveEndpointUrl cfg
      | _, _ => resolveEndpointUrl cfg
    | .bearer _ => resolveEndpointUrl cfg

/-- Whether a request requires Kimi / Coding Plan agent identification headers (`User-Agent: claude-cli/1.0.0`, `x-app: cli`). -/
private def needsCodingAgentHeaders (cfg : ModelConfig) (url : String) : Bool :=
  match cfg.provider, cfg.model with
  | .kimiCode, _ | .zaiCoding, _ | .alibabaCoding, _ => true
  | _, .kimi _ => true
  | .direct, .glm _ | .direct, .qwen _ => true
  | _, _ =>
    url.contains "api.kimi.com/coding" ||
    url.contains "dashscope.aliyuncs.com" ||
    url.contains "api.z.ai/api/coding"

/-- Build a `LeanHttp.Request` from a `ModelConfig`, resolved API key, and encoded JSON body. -/
def buildHttpRequest (cfg : ModelConfig) (apiKey : String) (payload : Json) : Except LLMError Request := do
  let url := resolveEndpointUrlWithAuth cfg (.apiKey apiKey)
  let target ← parseTarget url
  let mut req := (Request.post target)
    |>.json payload
    |>.header headerName!"Accept" headerValue!"application/json"
    |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }

  if needsCodingAgentHeaders cfg url then
    req := req
      |>.header headerName!"User-Agent" headerValue!"claude-cli/1.0.0"
      |>.header headerName!"x-app" headerValue!"cli"

  if url.contains "opencode.ai" then
    req := req.header headerName!"x-opencode-session" headerValue!"leanllm-session"

  match cfg.provider with
  | .direct =>
    match cfg.effectiveWireApi with
    | .anthropicMessages =>
      req ← setHeaderOrThrow req headerName!"x-api-key" apiKey
      req := req.header headerName!"anthropic-version" headerValue!"2023-06-01"
    | .geminiGenerate =>
      req ← setHeaderOrThrow req headerName!"x-goog-api-key" apiKey
    | _ =>
      if !apiKey.isEmpty then
        req := req.bearer apiKey
  | .openRouter orCfg =>
    req := req.bearer apiKey
    if let some referer := orCfg.siteUrl then
      req ← setHeaderOrThrow req headerName!"HTTP-Referer" referer
    if let some title := orCfg.appName then
      req ← setHeaderOrThrow req headerName!"X-Title" title
  | .bedrock _ =>
    req := req.bearer apiKey
  | .azure azCfg =>
    req ← setHeaderOrThrow req headerName!"api-key" apiKey
    req := req.param "api-version" azCfg.apiVersion
  | .custom _ | .ollama | .lmstudio =>
    if !apiKey.isEmpty then
      req := req.bearer apiKey
  | _ =>
    req := req.bearer apiKey

  return req

/-- Build a `LeanHttp.Request` from a `ModelConfig`, `ResolvedAuth`, and encoded JSON body. -/
def buildHttpRequestWithAuth (cfg : ModelConfig) (auth : ResolvedAuth) (payload : Json) :
    Except LLMError Request := do
  match auth with
  | .apiKey key =>
    buildHttpRequest cfg key payload
  | .bearer tok =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    return (Request.post target)
      |>.json payload
      |>.bearer tok
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
  | .openaiOAuth creds =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    let mut req := (Request.post target)
      |>.json payload
      |>.bearer creds.accessToken
      |>.header headerName!"Accept" headerValue!"text/event-stream"
      |>.header headerName!"originator" headerValue!"codex_cli_rs"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
    if let some accountId := creds.accountId then
      if !accountId.isEmpty then
        req ← setHeaderOrThrow req headerName!"chatgpt-account-id" accountId
    return req
  | .claudeOAuth creds =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    let mut req := (Request.post target)
      |>.json payload
      |>.bearer creds.accessToken
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.header headerName!"anthropic-version" headerValue!"2023-06-01"
      |>.header headerName!"x-app" headerValue!"cli"
      |>.header headerName!"anthropic-dangerous-direct-browser-access" headerValue!"true"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
    req ← setHeaderOrThrow req headerName!"User-Agent" Auth.Claude.claudeCliUserAgent
    req ← setHeaderOrThrow req headerName!"anthropic-beta" Auth.Claude.oauthBetaHeaders
    return req
  | .geminiOAuth creds _ =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    return (Request.post target)
      |>.json payload
      |>.bearer creds.accessToken
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
  | .antigravityOAuth creds _ =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    let mut req := (Request.post target)
      |>.json payload
      |>.bearer creds.accessToken
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.header headerName!"x-goog-api-client" headerValue!"google-cloud-sdk vscode_cloudshelleditor/0.1"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
    req ← setHeaderOrThrow req headerName!"User-Agent" "antigravity/1.18.3 darwin/arm64"
    req ← setHeaderOrThrow req headerName!"client-metadata" "{\"ideType\":\"ANTIGRAVITY\",\"platform\":\"PLATFORM_UNSPECIFIED\",\"pluginType\":\"GEMINI\"}"
    return req
  | .copilotBearer tok =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    return (Request.post target)
      |>.json payload
      |>.bearer tok
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.header headerName!"User-Agent" headerValue!"GithubCopilot/1.300.0"
      |>.header headerName!"Editor-Version" headerValue!"vscode/1.100.0"
      |>.header headerName!"Editor-Plugin-Version" headerValue!"copilot-chat/0.27.0"
      |>.header headerName!"Copilot-Integration-Id" headerValue!"vscode-chat"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }
  | .grokOAuth creds =>
    let url := resolveEndpointUrlWithAuth cfg auth
    let target ← parseTarget url
    return (Request.post target)
      |>.json payload
      |>.bearer creds.accessToken
      |>.header headerName!"Accept" headerValue!"application/json"
      |>.header headerName!"User-Agent" headerValue!"grok-cli/1.0.41"
      |>.header headerName!"X-XAI-Token-Auth" headerValue!"xai-grok-cli"
      |>.header headerName!"x-grok-client-version" headerValue!"1.0.41"
      |>.header headerName!"x-grok-client-identifier" headerValue!"grok-shell"
      |>.header headerName!"x-grok-client-surface" headerValue!"cli"
      |>.withTimeouts { connect := .ofNat 15000, total := .ofNat 120000 }

/-- Build a streaming `LeanHttp.Request` (for SSE) from a `ModelConfig`, `ResolvedAuth`, and encoded JSON body. -/
def buildStreamHttpRequestWithAuth (cfg : ModelConfig) (auth : ResolvedAuth) (payload : Json) :
    Except LLMError Request := do
  let req ← buildHttpRequestWithAuth cfg auth payload
  match cfg.effectiveWireApiForAuth auth with
  | .geminiGenerate =>
    let streamUrl := (resolveEndpointUrlWithAuth cfg auth).replace ":generateContent" ":streamGenerateContent"
    let target ← parseTarget streamUrl
    return ({ req with uri := target }).param "alt" "sse"
  | _ =>
    return req

/-- Build a streaming `LeanHttp.Request` (for SSE) from a `ModelConfig`, resolved API key, and encoded JSON body. -/
def buildStreamHttpRequest (cfg : ModelConfig) (apiKey : String) (payload : Json) : Except LLMError Request :=
  buildStreamHttpRequestWithAuth cfg (.apiKey apiKey) payload

/-- Execute a `LeanHttp.Request` (optionally reusing a `LeanHttp.Session`) and decode the JSON response. -/
def sendJson (req : Request) (session : Option Session := none) : ExceptT LLMError IO Json := do
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .ok json _     => return json
  | .status resp   => throw (fromHttpStatus resp)
  | .decode msg r  =>
    let raw := (String.fromUTF8? r.body).getD "<non-UTF8>"
    throw (.parse (.invalidJson raw msg) s!"Failed to parse JSON response: {msg}")
  | .transport err => throw (fromHttpError err)

/-- Execute a streaming Server-Sent Events `LeanHttp.Request` and invoke `onEvent` for each event. -/
def sendSse (req : Request) (session : Option Session := none)
    (onEvent : Sse.Event → IO Unit) : ExceptT LLMError IO Unit := do
  let req := req.header headerName!"Accept" headerValue!"text/event-stream"
  let outcome ← liftM <| match session with
    | some s => s.requestSse req onEvent
    | none   => LeanHttp.requestSse req {} onEvent
  match outcome with
  | .ok () _       => return ()
  | .status resp   => throw (fromHttpStatus resp)
  | .decode msg _  => throw (.parse (.invalidJson "<sse>" msg) msg)
  | .transport err => throw (fromHttpError err)

end Leanllm.Transport
