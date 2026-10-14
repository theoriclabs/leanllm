import Lean.Data.Json
import LeanHttp
import Leanllm.Auth

namespace Leanllm.Auth.OpenAI

open Lean LeanHttp Std.Http

def clientId : String := "app_EMoamEEZ73f0CkXaXp7hrann"
def tokenUrl : String := "https://auth.openai.com/oauth/token"
def chatGptResponsesUrl : String := "https://chatgpt.com/backend-api/codex/responses"
def originator : String := "codex_cli_rs"

private def getStr? (j : Json) (k : String) : Option String :=
  (j.getObjValAs? String k).toOption.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def getInt? (j : Json) (k : String) : Option Int :=
  (j.getObjValAs? Int k).toOption

private def enrichFromJwt (creds : OAuthCredentials) : OAuthCredentials :=
  let accountId := creds.accountId
    |>.or (creds.idToken >>= Auth.jwtChatGptAccountId?)
    |>.or (Auth.jwtChatGptAccountId? creds.accessToken)
  let email := creds.email
    |>.or (creds.idToken >>= Auth.jwtEmail?)
    |>.or (Auth.jwtEmail? creds.accessToken)
  let expiresAtMs := creds.expiresAtMs
    |>.or (Auth.jwtExpiresAtMs? creds.accessToken)
  { creds with accountId, email, expiresAtMs }

/-- Load OpenAI OAuth credentials from `~/.jcode/openai-auth.json`. -/
def loadFromJcodeFile (requestedAccount : Option String := none) : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".jcode/openai-auth.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  let some accounts := (root.getObjValAs? (Array Json) "openai_accounts").toOption | return none
  if accounts.isEmpty then return none
  let activeLabel := requestedAccount.or (getStr? root "active_openai_account")
  let chosen := match activeLabel with
    | some label => accounts.find? (fun a => getStr? a "label" == some label) |>.or accounts[0]?
    | none       => accounts[0]?
  let some acc := chosen | return none
  let some accessToken := getStr? acc "access_token" | return none
  let refreshToken := (getStr? acc "refresh_token").getD ""
  let label := (getStr? acc "label").getD "openai-1"
  return some <| enrichFromJwt {
    accessToken
    refreshToken
    idToken     := getStr? acc "id_token"
    accountId   := getStr? acc "account_id"
    email       := getStr? acc "email"
    expiresAtMs := getInt? acc "expires_at"
    source      := some (.jcodeOpenAiFile path label)
  }

/-- Load OpenAI OAuth credentials from `~/.codex/auth.json`. -/
def loadFromCodexCliFile : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".codex/auth.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  let tokObj := (root.getObjVal? "tokens").toOption.getD root
  let some accessToken := getStr? tokObj "access_token" | return none
  let refreshToken := (getStr? tokObj "refresh_token").getD ""
  if refreshToken.isEmpty && (getStr? tokObj "id_token").isNone then
    return none
  return some <| enrichFromJwt {
    accessToken
    refreshToken
    idToken     := getStr? tokObj "id_token"
    accountId   := getStr? tokObj "account_id"
    expiresAtMs := getInt? tokObj "expires_at"
    source      := some (.codexCliFile path)
  }

/-- Load OpenAI OAuth credentials from `~/.local/share/opencode/auth.json`. -/
def loadFromOpenCodeFile : IO (Option OAuthCredentials) := do
  let some path ← Auth.userHomePath ".local/share/opencode/auth.json" | return none
  let some root ← Auth.readJsonFile? path | return none
  for key in ["openai-codex", "openai_codex", "openai"] do
    if let .ok entry := root.getObjVal? key then
      if let some accessToken := (getStr? entry "access").or (getStr? entry "access_token") then
        let refreshToken := (getStr? entry "refresh").or (getStr? entry "refresh_token") |>.getD ""
        if !refreshToken.isEmpty then
          return some <| enrichFromJwt {
            accessToken
            refreshToken
            expiresAtMs := (getInt? entry "expires").or (getInt? entry "expires_at")
            source      := some (.opencodeFile path key)
          }
  return none

/-- Persist refreshed OpenAI OAuth credentials back to the source file they came from. -/
def saveRefreshedCredentials (creds : OAuthCredentials) : IO Unit := do
  match creds.source with
  | some (.codexCliFile path) =>
    if let some root ← Auth.readJsonFile? path then
      let tokObj := (root.getObjVal? "tokens").toOption.getD (Json.mkObj [])
      let tokObj := tokObj.setObjVal! "access_token" (toJson creds.accessToken)
      let tokObj := if creds.refreshToken.isEmpty then tokObj else tokObj.setObjVal! "refresh_token" (toJson creds.refreshToken)
      let tokObj := match creds.idToken with
        | some idt => tokObj.setObjVal! "id_token" (toJson idt)
        | none     => tokObj
      let tokObj := match creds.accountId with
        | some aid => tokObj.setObjVal! "account_id" (toJson aid)
        | none     => tokObj
      let updated := root.setObjVal! "tokens" tokObj
      Auth.writeJsonFileAtomic path updated
  | some (.jcodeOpenAiFile path label) =>
    if let some root ← Auth.readJsonFile? path then
      if let some accounts := (root.getObjValAs? (Array Json) "openai_accounts").toOption then
        let updatedAccounts := accounts.map fun acc =>
          if getStr? acc "label" == some label then
            let acc := acc.setObjVal! "access_token" (toJson creds.accessToken)
            let acc := if creds.refreshToken.isEmpty then acc else acc.setObjVal! "refresh_token" (toJson creds.refreshToken)
            let acc := match creds.expiresAtMs with
              | some exp => acc.setObjVal! "expires_at" (toJson exp)
              | none     => acc
            acc
          else acc
        Auth.writeJsonFileAtomic path (root.setObjVal! "openai_accounts" (toJson updatedAccounts))
  | _ => pure ()

/-- Refresh an OpenAI OAuth credential bundle using `POST https://auth.openai.com/oauth/token`. -/
def refreshCredentials (creds : OAuthCredentials) (session : Option Session := none) :
    ExceptT LLMError IO OAuthCredentials := do
  if creds.refreshToken.isEmpty then
    throw (.auth .invalidApiKey "OpenAI OAuth token is expired and has no refresh_token")
  let some uri := URI.parse? tokenUrl
    | throw (.invalidRequest (.malformedParameters tokenUrl) "Invalid OpenAI token URL")
  let req : Request := {
    method := .post
    uri    := .absolute uri
    body   := .form [
      ("grant_type", "refresh_token"),
      ("client_id", clientId),
      ("refresh_token", creds.refreshToken)
    ]
    timeouts := { connect := .ofNat 10000, total := .ofNat 30000 }
  }
  let outcome : Outcome Json ← liftM <| match session with
    | some s => s.requestAs req
    | none   => LeanHttp.requestAs req
  match outcome with
  | .transport err =>
    throw (.network (.transport err.detail) (toString err))
  | .status resp =>
    let raw := (String.fromUTF8? resp.body).getD "<non-UTF8>"
    throw (.auth .invalidApiKey s!"OpenAI OAuth token refresh failed (HTTP {resp.statusCode}): {raw}")
  | .decode msg _ =>
    throw (.parse (.invalidJson "<oauth>" msg) s!"OpenAI OAuth refresh decode failed: {msg}")
  | .ok j _ =>
    let some newAccess := getStr? j "access_token"
      | throw (.parse (.unexpectedSchema "OpenAI token response with access_token" j) "Missing access_token")
    let newRefresh := (getStr? j "refresh_token").getD creds.refreshToken
    let newIdToken := (getStr? j "id_token").or creds.idToken
    let expiresInSecs := (getInt? j "expires_in").getD 3600
    let now ← liftM Auth.nowMs
    let refreshed := enrichFromJwt {
      creds with
      accessToken  := newAccess
      refreshToken := newRefresh
      idToken      := newIdToken
      expiresAtMs  := some (now + expiresInSecs * 1000)
    }
    liftM (saveRefreshedCredentials refreshed)
    return refreshed

/-- Discover OpenAI subscription OAuth credentials from local stores and refresh if expiring within 5 minutes. -/
def loadAndEnsureFresh (requestedAccount : Option String := none) (session : Option Session := none) :
    ExceptT LLMError IO OAuthCredentials := do
  let candidates ← liftM <| do
    let c1 ← loadFromJcodeFile requestedAccount
    let c2 ← if requestedAccount.isNone then loadFromCodexCliFile else pure none
    let c3 ← if requestedAccount.isNone then loadFromOpenCodeFile else pure none
    return [c1, c2, c3].filterMap id
  let now ← liftM Auth.nowMs
  -- Prefer a non-expired candidate first; otherwise take the first candidate and refresh it.
  let freshCandidate := candidates.find? (fun c => !c.needsRefresh now 300000)
  match freshCandidate with
  | some c => return c
  | none =>
    let some first := candidates.head?
      | throw (.auth .missingApiKey "No OpenAI/Codex OAuth subscription credentials found in ~/.jcode/openai-auth.json or ~/.codex/auth.json")
    refreshCredentials first session

end Leanllm.Auth.OpenAI
