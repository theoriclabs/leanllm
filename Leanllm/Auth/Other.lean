import Lean.Data.Json
import LeanHttp
import Leanllm.Auth

namespace Leanllm.Auth.Other

open Lean LeanHttp Std.Http

private def getStr? (j : Json) (k : String) : Option String :=
  (j.getObjValAs? String k).toOption.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def getInt? (j : Json) (k : String) : Option Int :=
  (j.getObjValAs? Int k).toOption

/-! ## 1. Google Gemini Code Assist & Antigravity Subscription OAuth -/

def googleTokenUrl : String := "https://oauth2.googleapis.com/token"
def codeAssistEndpoint : String := "https://cloudcode-pa.googleapis.com/v1internal"
def antigravityEndpoint : String := "https://daily-cloudcode-pa.googleapis.com/v1internal"

private def requireEnv (name : String) : ExceptT LLMError IO String := do
  match ← liftM (IO.getEnv name) with
  | some v =>
    if v.trimAscii.toString.isEmpty then
      throw (.auth .missingApiKey s!"Environment variable {name} is empty")
    return v
  | none =>
    throw (.auth .missingApiKey s!"Set {name} to refresh Google OAuth tokens")

def loadGeminiOAuthFromDisk : IO (Option OAuthCredentials) := do
  -- Check ~/.gemini/oauth_creds.json first for id_token / client_id metadata
  let mut fallbackClientId : Option String := none
  if let some cliPath ← Auth.userHomePath ".gemini/oauth_creds.json" then
    if let some j ← Auth.readJsonFile? cliPath then
      fallbackClientId := (getStr? j "client_id").or (getStr? j "id_token" >>= Auth.jwtClientId?)
  -- 1. ~/.jcode/gemini_oauth.json
  if let some jcodePath ← Auth.userHomePath ".jcode/gemini_oauth.json" then
    if let some j ← Auth.readJsonFile? jcodePath then
      if let some access := getStr? j "access_token" then
        return some {
          accessToken  := access
          refreshToken := (getStr? j "refresh_token").getD ""
          clientId     := (getStr? j "client_id").or fallbackClientId
          expiresAtMs  := getInt? j "expires_at"
          email        := getStr? j "email"
          source       := some (.jcodeGeminiFile jcodePath)
        }
  -- 2. ~/.gemini/oauth_creds.json
  if let some cliPath ← Auth.userHomePath ".gemini/oauth_creds.json" then
    if let some j ← Auth.readJsonFile? cliPath then
      if let some access := getStr? j "access_token" then
        let idToken := getStr? j "id_token"
        return some {
          accessToken  := access
          refreshToken := (getStr? j "refresh_token").getD ""
          idToken
          clientId     := (getStr? j "client_id").or (idToken >>= Auth.jwtClientId?)
          expiresAtMs  := (getInt? j "expiry_date").or (getInt? j "expires_at")
          source       := some (.geminiCliFile cliPath)
        }
  return none

def loadAntigravityOAuthFromDisk : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".jcode/antigravity_oauth.json" | return none
  let some j ← Auth.readJsonFile? path | return none
  let some access := getStr? j "access_token" | return none
  let idToken := getStr? j "id_token"
  return some {
    accessToken  := access
    refreshToken := (getStr? j "refresh_token").getD ""
    idToken
    clientId     := (getStr? j "client_id").or (idToken >>= Auth.jwtClientId?)
    expiresAtMs  := getInt? j "expires_at"
    email        := getStr? j "email"
    projectId    := getStr? j "project_id"
    source       := some (.antigravityFile path)
  }

def refreshGoogleOAuth (clientId clientSecret : String) (creds : OAuthCredentials)
    (session : Option Session := none) : ExceptT LLMError IO OAuthCredentials := do
  if creds.refreshToken.isEmpty then
    throw (.auth .invalidApiKey "Google OAuth token is expired and has no refresh_token")
  let req : Request := {
    method := .post
    uri    := .absolute uri!"https://oauth2.googleapis.com/token"
    body   := .form [
      ("grant_type", "refresh_token"),
      ("client_id", clientId),
      ("client_secret", clientSecret),
      ("refresh_token", creds.refreshToken)
    ]
    timeouts := { connect := .ofNat 10000, total := .ofNat 20000 }
  }
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .transport err => throw (.network (.transport err.detail) (toString err))
  | .status resp =>
    let raw := (String.fromUTF8? resp.body).getD "<non-UTF8>"
    throw (.auth .invalidApiKey s!"Google OAuth refresh failed (HTTP {resp.statusCode}): {raw}")
  | .decode msg _ =>
    throw (.parse (.invalidJson "<google-oauth>" msg) msg)
  | .ok j _ =>
    let some newAccess := getStr? j "access_token"
      | throw (.parse (.unexpectedSchema "Google OAuth token response" j) "Missing access_token")
    let newRefresh := (getStr? j "refresh_token").getD creds.refreshToken
    let expiresInSecs := (getInt? j "expires_in").getD 3600
    let now ← liftM Auth.nowMs
    let refreshed := {
      creds with
      accessToken  := newAccess
      refreshToken := newRefresh
      expiresAtMs  := some (now + expiresInSecs * 1000)
    }
    if let some (.jcodeGeminiFile path) := creds.source then
      liftM <| Auth.writeJsonFileAtomic path (Json.mkObj [
        ("access_token", toJson refreshed.accessToken),
        ("refresh_token", toJson refreshed.refreshToken),
        ("expires_at", toJson (refreshed.expiresAtMs.getD 0))
      ])
    else if let some (.antigravityFile path) := creds.source then
      liftM <| Auth.writeJsonFileAtomic path (Json.mkObj <|
        [("access_token", toJson refreshed.accessToken),
         ("refresh_token", toJson refreshed.refreshToken),
         ("expires_at", toJson (refreshed.expiresAtMs.getD 0))] ++
        (match refreshed.projectId with | some p => [("project_id", toJson p)] | none => []))
    return refreshed

private def extractCompanionProject (j : Json) : Option String :=
  if let some s := getStr? j "cloudaicompanionProject" then
    some s
  else if let .ok obj := j.getObjVal? "cloudaicompanionProject" then
    getStr? obj "id"
  else
    none

/-- Resolve the `cloudaicompanionProject` ID from `v1internal:loadCodeAssist`. -/
def resolveCodeAssistProject (baseEndpoint : String) (accessToken : String) (ideType : String := "IDE_UNSPECIFIED")
    (session : Option Session := none) : ExceptT LLMError IO String := do
  if let some envProj ← liftM (IO.getEnv "GOOGLE_CLOUD_PROJECT") then
    if !envProj.trimAscii.toString.isEmpty then
      return envProj.trimAscii.toString
  let url := s!"{baseEndpoint}:loadCodeAssist"
  let some uri := URI.parse? url
    | throw (.invalidRequest (.malformedParameters url) "Invalid loadCodeAssist URL")
  let payload := Json.mkObj [
    ("metadata", Json.mkObj [
      ("ideType", toJson ideType),
      ("platform", "PLATFORM_UNSPECIFIED"),
      ("pluginType", "GEMINI")
    ])
  ]
  let req := (Request.post (.absolute uri))
    |>.json payload
    |>.bearer accessToken
    |>.header headerName!"Accept" headerValue!"application/json"
    |>.withTimeouts { connect := .ofNat 10000, total := .ofNat 20000 }
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .transport err => throw (.network (.transport err.detail) (toString err))
  | .status resp =>
    let raw := (String.fromUTF8? resp.body).getD "<non-UTF8>"
    throw (.auth .insufficientPermissions s!"loadCodeAssist failed (HTTP {resp.statusCode}): {raw}")
  | .decode msg _ => throw (.parse (.invalidJson "<loadCodeAssist>" msg) msg)
  | .ok j _ =>
    if let some proj := extractCompanionProject j then
      return proj
    throw (.auth .insufficientPermissions s!"loadCodeAssist did not return cloudaicompanionProject: {j.compress}")

def loadAndEnsureFreshGemini (session : Option Session := none) :
    ExceptT LLMError IO (OAuthCredentials × String) := do
  let some creds ← liftM loadGeminiOAuthFromDisk
    | throw (.auth .missingApiKey "No Gemini OAuth credentials found in ~/.jcode/gemini_oauth.json or ~/.gemini/oauth_creds.json")
  let now ← liftM Auth.nowMs
  let fresh ← if creds.needsRefresh now 60000 then
      let clientId ← match creds.clientId with
        | some cid => pure cid
        | none     => requireEnv "LEANLLM_GEMINI_OAUTH_CLIENT_ID"
      let clientSecret ← requireEnv "LEANLLM_GEMINI_OAUTH_CLIENT_SECRET"
      refreshGoogleOAuth clientId clientSecret creds session
    else
      pure creds
  let projectId ← match fresh.projectId with
    | some p => pure p
    | none   => resolveCodeAssistProject codeAssistEndpoint fresh.accessToken "IDE_UNSPECIFIED" session
  return ({ fresh with projectId := some projectId }, projectId)

def loadAndEnsureFreshAntigravity (session : Option Session := none) :
    ExceptT LLMError IO (OAuthCredentials × String) := do
  let some creds ← liftM loadAntigravityOAuthFromDisk
    | throw (.auth .missingApiKey "No Antigravity OAuth credentials found in ~/.jcode/antigravity_oauth.json")
  let now ← liftM Auth.nowMs
  let fresh ← if creds.needsRefresh now 60000 then
      let clientId ← match creds.clientId with
        | some cid => pure cid
        | none     => requireEnv "LEANLLM_ANTIGRAVITY_OAUTH_CLIENT_ID"
      let clientSecret ← requireEnv "LEANLLM_ANTIGRAVITY_OAUTH_CLIENT_SECRET"
      refreshGoogleOAuth clientId clientSecret creds session
    else
      pure creds
  let projectId ← match fresh.projectId with
    | some p => pure p
    | none   => resolveCodeAssistProject antigravityEndpoint fresh.accessToken "ANTIGRAVITY" session
  return ({ fresh with projectId := some projectId }, projectId)

/-! ## 2. GitHub Copilot Subscription OAuth Exchange -/

private def extractCopilotOAuthToken (j : Json) : Option String :=
  if let .ok obj := j.getObjVal? "github.com" then
    getStr? obj "oauth_token"
  else if let .ok kv := j.getObj? then
    kv.toList.findSome? fun (_, v) => getStr? v "oauth_token"
  else
    none

def loadGitHubCopilotToken : IO (Option String) := do
  if let some envTok ← IO.getEnv "GITHUB_COPILOT_TOKEN" then
    if !envTok.trimAscii.toString.isEmpty then
      return some envTok.trimAscii.toString
  for rel in [".config/github-copilot/hosts.json", ".config/github-copilot/apps.json"] do
    if let some path ← Auth.userHomePath rel then
      if let some j ← Auth.readJsonFile? path then
        if let some tok := extractCopilotOAuthToken j then
          return some tok
  return none

/-- Exchange a GitHub OAuth token for a short-lived Copilot API bearer token. -/
def exchangeCopilotBearerToken (session : Option Session := none) : ExceptT LLMError IO String := do
  let some ghToken ← liftM loadGitHubCopilotToken
    | throw (.auth .missingApiKey "No GitHub Copilot OAuth token found in ~/.config/github-copilot/hosts.json or GITHUB_COPILOT_TOKEN")
  let some authVal := Header.Value.ofString? s!"token {ghToken}"
    | throw (.invalidRequest (.malformedParameters "Invalid GitHub Copilot token") "Invalid token")
  let req := (Request.get uri!"https://api.github.com/copilot_internal/v2/token")
    |>.header headerName!"Authorization" authVal
    |>.header headerName!"User-Agent" headerValue!"GithubCopilot/1.300.0"
    |>.header headerName!"Editor-Version" headerValue!"vscode/1.100.0"
    |>.header headerName!"Editor-Plugin-Version" headerValue!"copilot-chat/0.27.0"
    |>.header headerName!"Accept" headerValue!"application/json"
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .transport err => throw (.network (.transport err.detail) (toString err))
  | .status resp =>
    let raw := (String.fromUTF8? resp.body).getD "<non-UTF8>"
    throw (.auth .invalidApiKey s!"GitHub Copilot token exchange failed (HTTP {resp.statusCode}): {raw}")
  | .decode msg _ => throw (.parse (.invalidJson "<copilot>" msg) msg)
  | .ok j _ =>
    let some token := getStr? j "token"
      | throw (.parse (.unexpectedSchema "Copilot token response" j) "Missing 'token' in Copilot response")
    return token

/-! ## 3. xAI Grok Build Subscription OIDC (`~/.grok/auth.json`) -/

private def findGrokOidcEntry (root : Json) : Option (String × Json) := do
  let kv ← root.getObj?.toOption
  kv.toList.find? fun (scopeKey, entry) =>
    scopeKey.startsWith "https://auth.x.ai::" || getStr? entry "auth_mode" == some "oidc"

def loadAndEnsureFreshGrok (session : Option Session := none) : ExceptT LLMError IO OAuthCredentials := do
  let some path ← liftM (Auth.userHomePath ".grok/auth.json")
    | throw (.auth .missingApiKey "Cannot resolve ~/.grok/auth.json")
  let some root ← liftM (Auth.readJsonFile? path)
    | throw (.auth .missingApiKey "No Grok Build credentials found in ~/.grok/auth.json")
  let some (scopeKey, entry) := findGrokOidcEntry root
    | throw (.auth .missingApiKey "No OIDC scope entry found in ~/.grok/auth.json")
  let some key := getStr? entry "key"
    | throw (.auth .missingApiKey "Missing 'key' in ~/.grok/auth.json")
  let scopeClientId := match scopeKey.splitOn "::" with
    | [_, cid] => if cid.isEmpty then none else some cid
    | _        => none
  let clientId := (getStr? entry "oidc_client_id")
    |>.or scopeClientId
    |>.or (Auth.jwtClientId? key)
  let refresh := (getStr? entry "refresh_token").getD ""
  let expiresAtMs := Auth.jwtExpiresAtMs? key
  let creds : OAuthCredentials := {
    accessToken  := key
    refreshToken := refresh
    clientId
    expiresAtMs
    source       := some (.grokAuthFile path)
  }
  let now ← liftM Auth.nowMs
  if !creds.needsRefresh now 60000 then
    return creds
  let resolvedClientId ← match creds.clientId with
    | some cid => pure cid
    | none     =>
      if let some envCid ← liftM (Auth.firstEnvVar? ["LEANLLM_GROK_CLIENT_ID"]) then
        pure envCid
      else
        throw (.auth .missingApiKey "Missing oidc_client_id in ~/.grok/auth.json; set LEANLLM_GROK_CLIENT_ID")
  let req : Request := {
    method := .post
    uri    := .absolute uri!"https://auth.x.ai/oauth2/token"
    body   := .form [
      ("grant_type", "refresh_token"),
      ("refresh_token", creds.refreshToken),
      ("client_id", resolvedClientId)
    ]
  }
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .ok j _ =>
    let some newAccess := getStr? j "access_token" | return creds
    let newRefresh := (getStr? j "refresh_token").getD creds.refreshToken
    return {
      creds with
      accessToken  := newAccess
      refreshToken := newRefresh
      expiresAtMs  := Auth.jwtExpiresAtMs? newAccess
    }
  | _ => return creds

/-! ## 4. Cross-Tool Credential Store Lookup (DeepSeek, Kimi, Z.AI, MiniMax, Qwen, etc.) -/

private def parseDotEnvValue (content : String) (envKey : String) : Option String := do
  let prefixStr := s!"{envKey}="
  for rawLine in content.splitOn "\n" do
    let line := rawLine.trimAscii.toString
    if line.startsWith prefixStr then
      let val := (line.drop prefixStr.length).toString.trimAscii.toString
      let val :=
        if (val.startsWith "\"" && val.endsWith "\"") || (val.startsWith "'" && val.endsWith "'") then
          ((val.drop 1).dropEnd 1).toString
        else
          val
      if !val.isEmpty then
        return val
  none

/-- Read a saved API key from `~/.config/jcode/<envFile>` or `~/Library/Application Support/jcode/<envFile>`. -/
def loadFromJcodeEnvFile? (envKey : String) (envFile : String) : IO (Option String) := do
  for baseRel in [".config/jcode", "Library/Application Support/jcode", ".jcode"] do
    if let some dir ← Auth.userHomePath baseRel then
      let path := dir / envFile
      if ← path.pathExists then
        try
          let content ← IO.FS.readFile path
          if let some val := parseDotEnvValue content envKey then
            return some val
        catch _ => pure ()
  return none

/-- Provider keys used in OpenCode (`~/.local/share/opencode/auth.json`), Pi (`~/.pi/agent/auth.json`),
    OpenClaw (`~/.openclaw/agent/auth.json`), and Hermes (`~/.hermes/auth.json`). -/
def externalProviderKeysForEnv (envKey : String) : List String :=
  match envKey with
  | "DEEPSEEK_API_KEY"            => ["deepseek"]
  | "KIMI_API_KEY"                => ["kimi-coding", "kimi", "moonshotai", "moonshot"]
  | "MOONSHOT_API_KEY"            => ["moonshotai", "moonshot", "kimi"]
  | "ZHIPU_API_KEY" | "ZAI_API_KEY" => ["zai", "zhipu"]
  | "MINIMAX_API_KEY"             => ["minimax", "minimax-cn"]
  | "BAILIAN_CODING_PLAN_API_KEY" | "DASHSCOPE_API_KEY" => ["alibaba-coding-plan", "bailian", "qwen"]
  | "XAI_API_KEY"                 => ["xai"]
  | "MISTRAL_API_KEY"             => ["mistral"]
  | "GROQ_API_KEY"                => ["groq"]
  | "CEREBRAS_API_KEY"            => ["cerebras"]
  | "TOGETHER_API_KEY"            => ["togetherai", "together-ai", "together"]
  | "FIREWORKS_API_KEY"           => ["fireworks"]
  | "DEEPINFRA_API_KEY"           => ["deepinfra"]
  | "NOVITA_API_KEY"              => ["novita", "novita-ai", "novita.ai"]
  | "BASETEN_API_KEY"             => ["baseten"]
  | "NEBIUS_API_KEY"              => ["nebius"]
  | "NVIDIA_API_KEY"              => ["nvidia-nim", "nvidia"]
  | "OPENCODE_API_KEY"            => ["opencode"]
  | "OPENCODE_GO_API_KEY"         => ["opencode-go", "opencode"]
  | "OPENROUTER_API_KEY"          => ["openrouter"]
  | "ANTHROPIC_API_KEY"           => ["anthropic", "claude"]
  | "OPENAI_API_KEY"              => ["openai", "openai-api"]
  | "GEMINI_API_KEY" | "GOOGLE_API_KEY" => ["google", "gemini"]
  | _                             => []

private def extractExternalEntryKey (entry : Json) : Option String :=
  (getStr? entry "key")
    |>.or (getStr? entry "access_token")
    |>.or (getStr? entry "access")

/-- Probe OpenCode, Pi, OpenClaw, and Hermes auth stores for a matching provider credential. -/
def loadFromExternalAgentStores? (envKey : String) : IO (Option String) := do
  let providerKeys := externalProviderKeysForEnv envKey
  if providerKeys.isEmpty then return none
  for rel in [
    ".local/share/opencode/auth.json",
    ".pi/agent/auth.json",
    ".openclaw/agent/auth.json",
    ".hermes/auth.json"
  ] do
    if let some path ← Auth.userHomePath rel then
      if let some root ← Auth.readJsonFile? path then
        -- 1. Direct flat map `{ "<provider>": { "key": "..." } }`
        for pk in providerKeys do
          if let .ok entry := root.getObjVal? pk then
            if let some k := extractExternalEntryKey entry then
              return some k
        -- 2. Hermes `credential_pool.<provider>[0]`
        if let .ok pool := root.getObjVal? "credential_pool" then
          for pk in providerKeys do
            if let .ok (arr : Array Json) := pool.getObjValAs? (Array Json) pk then
              if let some first := arr[0]? then
                if let some k := extractExternalEntryKey first then
                  return some k
  return none

end Leanllm.Auth.Other
