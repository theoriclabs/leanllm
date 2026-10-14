import Lean.Data.Json
import LeanHttp
import Leanllm.Auth

namespace Leanllm.Auth.Claude

open Lean LeanHttp Std.Http

def tokenUrl : String := "https://platform.claude.com/v1/oauth/token"
def oauthMessagesUrl : String := "https://api.anthropic.com/v1/messages?beta=true"
def refreshScopes : String := "user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload"
def claudeCliUserAgent : String := "claude-cli/2.1.280 (external, sdk-cli)"
def oauthBetaHeaders : String := "claude-code-20250219,oauth-2025-04-20,interleaved-thinking-2025-05-14,context-management-2025-06-27,prompt-caching-scope-2026-01-05,advisor-tool-2026-03-01,advanced-tool-use-2025-11-20,effort-2025-11-24"
def oauthBillingHeader : String := "x-anthropic-billing-header: cc_version=2.1.280; cc_entrypoint=sdk-cli; cch=33f85;"
def claudeCodeIdentity : String := "You are a Claude agent, built on Anthropic's Claude Agent SDK."

/-- Map tool names to Claude Code's PascalCase builtin convention for OAuth requests. -/
def mapToolNameForOAuth : String → String
  | "bash"         => "Bash"
  | "read"         => "Read"
  | "write"        => "Write"
  | "edit"         => "Edit"
  | "glob"         => "Glob"
  | "grep"         => "Grep"
  | "subagent"     => "Agent"
  | "schedule"     => "ScheduleWakeup"
  | "skill_manage" => "Skill"
  | other          => other

/-- Map Claude Code's PascalCase builtin tool names back to standard names on OAuth responses. -/
def mapToolNameFromOAuth : String → String
  | "Bash"           => "bash"
  | "Read"           => "read"
  | "Write"          => "write"
  | "Edit"           => "edit"
  | "Glob"           => "glob"
  | "Grep"           => "grep"
  | "Agent"          => "subagent"
  | "ScheduleWakeup" => "schedule"
  | "Skill"          => "skill_manage"
  | other            => other

private def getStr? (j : Json) (k : String) : Option String :=
  (j.getObjValAs? String k).toOption.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def getInt? (j : Json) (k : String) : Option Int :=
  (j.getObjValAs? Int k).toOption

private def parseScopes (j : Json) (key : String) : List String :=
  if let .ok (arr : Array String) := j.getObjValAs? (Array String) key then
    arr.toList
  else if let some s := getStr? j key then
    s.splitOn " " |>.filter (!·.isEmpty)
  else
    []

private def parseClaudeOAuthObj (j : Json) (source : CredentialSource) : Option OAuthCredentials := do
  let obj := (j.getObjVal? "claudeAiOauth").toOption.getD j
  let accessToken ← (getStr? obj "accessToken").or (getStr? obj "access")
  let refreshToken := (getStr? obj "refreshToken").or (getStr? obj "refresh") |>.getD ""
  let clientId := (getStr? obj "clientId")
    |>.or (getStr? obj "client_id")
    |>.or (Auth.jwtClientId? accessToken)
  let expiresAtMs := (getInt? obj "expiresAt").or (getInt? obj "expires")
  let subscriptionType := (getStr? obj "subscriptionType").or (getStr? obj "subscription_type")
  let scopes := parseScopes obj "scopes"
  return {
    accessToken
    refreshToken
    clientId
    expiresAtMs
    subscriptionType
    scopes
    source := some source
  }

/-- Resolve the Claude OAuth `client_id` from `creds` or environment variables. -/
def resolveClientId (creds : OAuthCredentials) : ExceptT LLMError IO String := do
  if let some cid := creds.clientId then
    if !cid.isEmpty then return cid
  if let some envCid ← liftM (Auth.firstEnvVar? ["LEANLLM_CLAUDE_CLIENT_ID", "CLAUDE_OAUTH_CLIENT_ID"]) then
    return envCid
  throw (.auth .missingApiKey
    "Claude OAuth token is expired and no client_id is configured; set LEANLLM_CLAUDE_CLIENT_ID")

/-- Load Claude OAuth credentials from `$CLAUDE_CODE_OAUTH_TOKEN`. -/
def loadFromEnv : IO (Option OAuthCredentials) := do
  let some raw ← IO.getEnv "CLAUDE_CODE_OAUTH_TOKEN" | return none
  let trimmed := raw.trimAscii.toString
  if trimmed.isEmpty then return none
  if let .ok j := Json.parse trimmed then
    return parseClaudeOAuthObj j .claudeCodeEnv
  if !trimmed.contains '{' then
    return some { accessToken := trimmed, source := some .claudeCodeEnv }
  return none

/-- Load Claude OAuth credentials from `~/.jcode/auth.json`. -/
def loadFromJcodeFile (requestedAccount : Option String := none) : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".jcode/auth.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  let some accounts := (root.getObjValAs? (Array Json) "anthropic_accounts").toOption | return none
  if accounts.isEmpty then return none
  let activeLabel := requestedAccount.or (getStr? root "active_anthropic_account")
  let chosen := match activeLabel with
    | some label => accounts.find? (fun a => getStr? a "label" == some label) |>.or accounts[0]?
    | none       => accounts[0]?
  let some acc := chosen | return none
  let label := (getStr? acc "label").getD "claude-1"
  return parseClaudeOAuthObj acc (.jcodeClaudeFile path label)

/-- Load Claude OAuth credentials from `~/.claude/.credentials.json`. -/
def loadFromClaudeCodeFile : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".claude/.credentials.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  return parseClaudeOAuthObj root (.claudeCodeFile path)

/-- Load Claude OAuth credentials from macOS Keychain (`Claude Code-credentials`). -/
def loadFromKeychain : IO (Option OAuthCredentials) := do
  try
    let out ← IO.Process.output {
      cmd  := "/usr/bin/security"
      args := #["find-generic-password", "-s", "Claude Code-credentials", "-w"]
    }
    if out.exitCode != 0 then return none
    let trimmed := out.stdout.trimAscii.toString
    if trimmed.isEmpty then return none
    let .ok j := Json.parse trimmed | return none
    return parseClaudeOAuthObj j .claudeCodeKeychain
  catch _ =>
    return none

/-- Load Claude OAuth credentials from `~/.local/share/opencode/auth.json`. -/
def loadFromOpenCodeFile : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".local/share/opencode/auth.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  for key in ["anthropic", "claude"] do
    if let .ok entry := root.getObjVal? key then
      if let some creds := parseClaudeOAuthObj entry (.opencodeFile path key) then
        return some creds
  return none

/-- Persist rotated Claude OAuth tokens back to the source file on disk. -/
def saveRefreshedCredentials (creds : OAuthCredentials) : IO Unit := do
  match creds.source with
  | some (.jcodeClaudeFile path label) =>
    if let some root ← Auth.readJsonFile? path then
      if let some accounts := (root.getObjValAs? (Array Json) "anthropic_accounts").toOption then
        let updatedAccounts := accounts.map fun acc =>
          if getStr? acc "label" == some label then
            let acc := acc.setObjVal! "access" (toJson creds.accessToken)
            let acc := if creds.refreshToken.isEmpty then acc else acc.setObjVal! "refresh" (toJson creds.refreshToken)
            let acc := match creds.expiresAtMs with
              | some exp => acc.setObjVal! "expires" (toJson exp)
              | none     => acc
            acc
          else acc
        Auth.writeJsonFileAtomic path (root.setObjVal! "anthropic_accounts" (toJson updatedAccounts))
  | some (.claudeCodeFile path) =>
    if let some root ← Auth.readJsonFile? path then
      let inner := (root.getObjVal? "claudeAiOauth").toOption.getD (Json.mkObj [])
      let inner := inner.setObjVal! "accessToken" (toJson creds.accessToken)
      let inner := if creds.refreshToken.isEmpty then inner else inner.setObjVal! "refreshToken" (toJson creds.refreshToken)
      let inner := match creds.expiresAtMs with
        | some exp => inner.setObjVal! "expiresAt" (toJson exp)
        | none     => inner
      Auth.writeJsonFileAtomic path (root.setObjVal! "claudeAiOauth" inner)
  | _ => pure ()

private def sendRefreshRequest (clientId refreshToken : String) (scope : Option String) (session : Option Session) :
    IO (Outcome Json) := do
  let fields : List (String × Json) :=
    [("grant_type", toJson "refresh_token"),
     ("refresh_token", toJson refreshToken),
     ("client_id", toJson clientId)] ++
    match scope with
    | some s => [("scope", toJson s)]
    | none   => []
  let req := (Request.post uri!"https://platform.claude.com/v1/oauth/token")
    |>.json (Json.mkObj fields)
    |>.header headerName!"Accept" headerValue!"application/json"
    |>.withTimeouts { connect := .ofNat 10000, total := .ofNat 20000 }
  match session with
  | some s => s.requestAs req
  | none   => LeanHttp.requestAs req

/-- Refresh a Claude OAuth token bundle via `https://platform.claude.com/v1/oauth/token`. -/
def refreshCredentials (creds : OAuthCredentials) (session : Option Session := none) :
    ExceptT LLMError IO OAuthCredentials := do
  if creds.refreshToken.isEmpty then
    throw (.auth .invalidApiKey "Claude OAuth token is expired and has no refresh_token")
  let clientId ← resolveClientId creds
  let firstOutcome ← liftM <| sendRefreshRequest clientId creds.refreshToken (some refreshScopes) session
  let outcome ← match firstOutcome with
    | .status resp =>
      let body := (String.fromUTF8? resp.body).getD ""
      if body.contains "invalid_scope" || body.contains "scope" then
        liftM <| sendRefreshRequest clientId creds.refreshToken none session
      else
        pure firstOutcome
    | other => pure other
  match outcome with
  | .transport err =>
    throw (.network (.transport err.detail) (toString err))
  | .status resp =>
    let raw := (String.fromUTF8? resp.body).getD "<non-UTF8>"
    throw (.auth .invalidApiKey s!"Claude OAuth token refresh failed (HTTP {resp.statusCode}): {raw}")
  | .decode msg _ =>
    throw (.parse (.invalidJson "<oauth>" msg) s!"Claude OAuth refresh decode failed: {msg}")
  | .ok j _ =>
    let some newAccess := getStr? j "access_token"
      | throw (.parse (.unexpectedSchema "Claude token response with access_token" j) "Missing access_token")
    let newRefresh := (getStr? j "refresh_token").getD creds.refreshToken
    let expiresInSecs := (getInt? j "expires_in").getD 3600
    let now ← liftM Auth.nowMs
    let refreshed : OAuthCredentials := {
      creds with
      accessToken  := newAccess
      refreshToken := newRefresh
      expiresAtMs  := some (now + expiresInSecs * 1000)
    }
    liftM (saveRefreshedCredentials refreshed)
    return refreshed

/-- Discover Claude subscription OAuth credentials from local stores and refresh if expiring within 5 minutes. -/
def loadAndEnsureFresh (requestedAccount : Option String := none) (session : Option Session := none) :
    ExceptT LLMError IO OAuthCredentials := do
  let candidates ← liftM <| do
    let c0 ← if requestedAccount.isNone then loadFromEnv else pure none
    let c1 ← loadFromJcodeFile requestedAccount
    let c2 ← if requestedAccount.isNone then loadFromClaudeCodeFile else pure none
    let c3 ← if requestedAccount.isNone then loadFromKeychain else pure none
    let c4 ← if requestedAccount.isNone then loadFromOpenCodeFile else pure none
    return [c0, c1, c2, c3, c4].filterMap id
  let now ← liftM Auth.nowMs
  let freshCandidate := candidates.find? (fun c => !c.needsRefresh now 300000 && c.expiresAtMs.all (· > now))
  match freshCandidate with
  | some c => return c
  | none =>
    let some first := candidates.head?
      | throw (.auth .missingApiKey "No Claude OAuth subscription credentials found in ~/.jcode/auth.json, ~/.claude/.credentials.json, or macOS Keychain")
    refreshCredentials first session

end Leanllm.Auth.Claude
