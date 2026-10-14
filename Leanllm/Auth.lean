import Lean.Data.Json
import LeanHttp
import Leanllm.Error

namespace Leanllm

open Lean

/-- How the caller wants to authenticate with the provider. -/
inductive AuthMode where
  | auto                                     -- Use explicit apiKey / env var if present, otherwise subscription OAuth
  | oauth  (account : Option String := none) -- Force subscription OAuth (optionally pinned to a named account)
  | apiKey (key : Option String := none)     -- Force per-token API key
  | bearer (token : String)                  -- Raw bearer token
  deriving Repr, BEq, Inhabited

/-- Where a subscription OAuth credential was loaded from, so rotated tokens can be saved back. -/
inductive CredentialSource where
  | codexCliFile     (path : System.FilePath)
  | jcodeOpenAiFile  (path : System.FilePath) (account : String)
  | claudeCodeFile   (path : System.FilePath)
  | claudeCodeKeychain
  | claudeCodeEnv
  | jcodeClaudeFile  (path : System.FilePath) (account : String)
  | geminiCliFile    (path : System.FilePath)
  | jcodeGeminiFile  (path : System.FilePath)
  | antigravityFile  (path : System.FilePath)
  | copilotHostsFile (path : System.FilePath)
  | grokAuthFile     (path : System.FilePath)
  | opencodeFile     (path : System.FilePath) (providerKey : String)
  deriving Repr, BEq, Inhabited

/-- Normalized OAuth token bundle for a subscription login. -/
structure OAuthCredentials where
  accessToken      : String
  refreshToken     : String                  := ""
  idToken          : Option String           := none
  accountId        : Option String           := none
  email            : Option String           := none
  expiresAtMs      : Option Int              := none
  subscriptionType : Option String           := none
  projectId        : Option String           := none
  scopes           : List String             := []
  source           : Option CredentialSource := none
  deriving Repr, BEq, Inhabited

/-- Concrete credential resolved for an outgoing LLM request. -/
inductive ResolvedAuth where
  | apiKey          (key : String)
  | openaiOAuth     (creds : OAuthCredentials)
  | claudeOAuth     (creds : OAuthCredentials)
  | geminiOAuth     (creds : OAuthCredentials) (projectId : String)
  | antigravityOAuth (creds : OAuthCredentials) (projectId : String)
  | copilotBearer   (token : String)
  | grokOAuth       (creds : OAuthCredentials)
  | bearer          (token : String)
  deriving Repr, BEq, Inhabited

namespace Auth

/-- Current UNIX epoch time in milliseconds. -/
def nowMs : IO Int := do
  let ts ← Std.Time.Timestamp.now
  return ts.toMillisecondsSinceUnixEpoch.val

/-- Whether an `OAuthCredentials` bundle is expired or within `skewMs` of expiring. -/
def OAuthCredentials.needsRefresh (creds : OAuthCredentials) (currentMs : Int) (skewMs : Int := 60000) : Bool :=
  match creds.expiresAtMs with
  | some exp => exp <= currentMs + skewMs && !creds.refreshToken.isEmpty
  | none     => false

/-- Resolve a path relative to the user's home directory (`$HOME`). -/
def userHomePath (rel : String) : IO (Option System.FilePath) := do
  let some home ← IO.getEnv "HOME" | return none
  return some (System.FilePath.mk home / rel)

/-- Read and parse a JSON file if it exists on disk. -/
def readJsonFile? (path : System.FilePath) : IO (Option Json) := do
  if !(← path.pathExists) then return none
  try
    let content ← IO.FS.readFile path
    return (Json.parse content).toOption
  catch _ =>
    return none

/-- Write a JSON value to a file atomically (via a temporary sibling file). -/
def writeJsonFileAtomic (path : System.FilePath) (j : Json) : IO Unit := do
  try
    if let some parent := path.parent then
      IO.FS.createDirAll parent
    let tmpPath : System.FilePath := ⟨s!"{path}.tmp"⟩
    IO.FS.writeFile tmpPath (j.pretty ++ "\n")
    IO.FS.rename tmpPath path
  catch _ =>
    pure ()

/-! ## Base64URL and JWT Claim Decoding -/

private def base64UrlCharVal (c : Char) : Option UInt8 :=
  if 'A' ≤ c && c ≤ 'Z' then some (c.toNat - 'A'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'z' then some (c.toNat - 'a'.toNat + 26).toUInt8
  else if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat + 52).toUInt8
  else if c == '-' || c == '+' then some 62
  else if c == '_' || c == '/' then some 63
  else none

/-- Decode a Base64 or Base64URL string (with or without `=` padding) into a `ByteArray`. -/
def decodeBase64Url? (input : String) : Option ByteArray := do
  let chars := input.trimAscii.toString.toList.filter (· != '=')
  let mut vals : Array UInt8 := #[]
  for c in chars do
    let v ← base64UrlCharVal c
    vals := vals.push v
  let mut out := ByteArray.empty
  let mut i := 0
  while i + 1 < vals.size do
    let b0 := vals[i]!
    let b1 := vals[i + 1]!
    out := out.push ((b0 <<< 2) ||| (b1 >>> 4))
    if i + 2 < vals.size then
      let b2 := vals[i + 2]!
      out := out.push (((b1 &&& 0x0F) <<< 4) ||| (b2 >>> 2))
      if i + 3 < vals.size then
        let b3 := vals[i + 3]!
        out := out.push (((b2 &&& 0x03) <<< 6) ||| b3)
    i := i + 4
  return out

/-- Decode the payload (second dot-separated segment) of a JWT into `Lean.Json`. -/
def decodeJwtPayload? (token : String) : Option Json := do
  let parts := token.trimAscii.toString.splitOn "."
  let payloadB64 ← parts[1]?
  let bytes ← decodeBase64Url? payloadB64
  let text ← String.fromUTF8? bytes
  (Json.parse text).toOption

/-- Extract the `exp` claim from a JWT as epoch milliseconds. -/
def jwtExpiresAtMs? (token : String) : Option Int := do
  let payload ← decodeJwtPayload? token
  let expSecs ← (payload.getObjValAs? Int "exp").toOption
  return expSecs * 1000

/-- Extract the `email` claim from a JWT. -/
def jwtEmail? (token : String) : Option String := do
  let payload ← decodeJwtPayload? token
  (payload.getObjValAs? String "email").toOption

/-- Extract `chatgpt_account_id` from an OpenAI `id_token` or `access_token` JWT. -/
def jwtChatGptAccountId? (token : String) : Option String := do
  let payload ← decodeJwtPayload? token
  let authObj ← (payload.getObjVal? "https://api.openai.com/auth").toOption
  (authObj.getObjValAs? String "chatgpt_account_id").toOption

end Auth
end Leanllm
