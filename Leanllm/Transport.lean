import LeanHttp
import Lean.Data.Json
import Leanllm.Provider
import Leanllm.Models
import Leanllm.Error

namespace Leanllm.Transport

open Lean LeanHttp Std.Http

/-- Default environment variable names for API keys per `(Provider, Model)`. -/
def apiKeyEnvVars (provider : Provider) (model : Model) : List String :=
  match provider, model with
  | .direct,       .claude _ => ["ANTHROPIC_API_KEY"]
  | .direct,       .openai _ => ["OPENAI_API_KEY"]
  | .direct,       .gemini _ => ["GEMINI_API_KEY", "GOOGLE_API_KEY"]
  | .openRouter _, _         => ["OPENROUTER_API_KEY"]
  | .bedrock _,    _         => ["AWS_BEARER_TOKEN_BEDROCK", "BEDROCK_API_KEY"]
  | .azure _,      _         => ["AZURE_OPENAI_API_KEY", "OPENAI_API_KEY"]
  | .vertex _,     _         => ["VERTEX_API_KEY", "GOOGLE_ACCESS_TOKEN"]
  | .custom _,     _         => ["LLM_API_KEY", "OPENAI_API_KEY"]

/-- Resolve the API key from `cfg.apiKey` or the environment. -/
def resolveApiKey (cfg : ModelConfig) : ExceptT LLMError IO String := do
  if let some key := cfg.apiKey then
    if !key.isEmpty then return key
  let envVars := apiKeyEnvVars cfg.provider cfg.model
  for var in envVars do
    if let some val ← liftM (IO.getEnv var) then
      if !val.isEmpty then return val
  match cfg.provider with
  | .custom _ => return ""
  | _ =>
    let varsStr := String.intercalate " or " envVars
    throw (.auth .missingApiKey s!"Missing API key: set ModelConfig.apiKey or environment variable {varsStr}")

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

private def parseRetryAfterMs (headers : Headers) : Option Nat := do
  let val ← headers.get? headerName!"retry-after"
  let secs ← (toString val).trimAscii.toString.toNat?
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
    let retryMs := parseRetryAfterMs resp.headers
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

private def setValidatedHeader (req : Request) (name : Header.Name) (rawValue : String) :
    Except LLMError Request := do
  let some val := Header.Value.ofString? rawValue
    | throw (.invalidRequest (.malformedParameters s!"Invalid HTTP header value for {name}") "Invalid header value")
  return req.header name val

private def parseTarget (url : String) : Except LLMError Target := do
  let some uri := URI.parse? url
    | throw (.invalidRequest (.malformedParameters s!"Invalid endpoint URL: {url}") "Invalid endpoint URL")
  return .absolute uri

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
      match api with
      | .anthropicMessages => "https://api.anthropic.com/v1/messages"
      | .openaiChat        => "https://api.openai.com/v1/chat/completions"
      | .openaiResponses   => "https://api.openai.com/v1/responses"
      | .geminiGenerate    => s!"https://generativelanguage.googleapis.com/v1beta/models/{modelId}:generateContent"
      | .bedrockConverse   => s!"https://bedrock-runtime.us-east-1.amazonaws.com/model/{modelId}/converse"
      | .bedrockInvoke     => s!"https://bedrock-runtime.us-east-1.amazonaws.com/model/{modelId}/invoke"
      | .auto              => "https://api.openai.com/v1/chat/completions"
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
      | .openai _ => s!"{host}/endpoints/openapi/chat/completions"
    | .custom base =>
      let trimmed := if base.endsWith "/" then (base.dropEnd 1).toString else base
      match api with
      | .anthropicMessages => s!"{trimmed}/v1/messages"
      | .openaiResponses   => s!"{trimmed}/v1/responses"
      | _                  => s!"{trimmed}/chat/completions"

/-- Build a `LeanHttp.Request` from a `ModelConfig`, resolved API key, and encoded JSON body. -/
def buildHttpRequest (cfg : ModelConfig) (apiKey : String) (payload : Json) : Except LLMError Request := do
  let url := resolveEndpointUrl cfg
  let target ← parseTarget url
  let mut req := (Request.post target)
    |>.json payload
    |>.header headerName!"Accept" headerValue!"application/json"
  req := { req with timeouts := { connect := .ofNat 15000, total := .ofNat 120000 } }

  match cfg.provider with
  | .direct =>
    match cfg.effectiveWireApi with
    | .anthropicMessages =>
      req ← setValidatedHeader req headerName!"x-api-key" apiKey
      req := req.header headerName!"anthropic-version" headerValue!"2023-06-01"
    | .geminiGenerate =>
      req ← setValidatedHeader req headerName!"x-goog-api-key" apiKey
    | _ =>
      req := req.bearer apiKey
  | .openRouter orCfg =>
    req := req.bearer apiKey
    if let some referer := orCfg.siteUrl then
      req ← setValidatedHeader req headerName!"HTTP-Referer" referer
    if let some title := orCfg.appName then
      req ← setValidatedHeader req headerName!"X-Title" title
  | .bedrock _ =>
    req := req.bearer apiKey
  | .azure azCfg =>
    req ← setValidatedHeader req headerName!"api-key" apiKey
    req := req.param "api-version" azCfg.apiVersion
  | .vertex _ =>
    req := req.bearer apiKey
  | .custom _ =>
    if !apiKey.isEmpty then
      req := req.bearer apiKey

  return req

/-- Execute a `LeanHttp.Request` (optionally reusing a `LeanHttp.Session`) and decode the JSON response. -/
def sendJson (req : Request) (session : Option Session := none) : ExceptT LLMError IO Json := do
  let httpResult ← liftM <| match session with
    | some s => s.request req
    | none   => LeanHttp.request req
  let resp ← match httpResult with
    | .error err => throw (fromHttpError err)
    | .ok resp   => pure resp
  if !resp.isSuccess then
    throw (fromHttpStatus resp)
  let some bodyStr := String.fromUTF8? resp.body
    | throw (.parse (.invalidJson "<non-UTF8>" "Response body is not valid UTF-8") "Response body is not valid UTF-8")
  match Json.parse bodyStr with
  | .ok json   => return json
  | .error err => throw (.parse (.invalidJson bodyStr err) s!"Failed to parse JSON response: {err}")

end Leanllm.Transport
