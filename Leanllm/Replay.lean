import Lean.Data.Json
import Leanllm.Basic
import Leanllm.Codec
import Leanllm.Transport
import Leanllm.Sha256
import Leanllm.ResponseJson

/-!
# Record/replay transport keyed by request hash

`Recorder.wrap dir inner` calls `inner` and stores each successful response under
`<dir>/<requestHash>.json`. `Replayer.caller dir` serves those responses by hash without touching
the network. A replay miss is an error naming the hash and model, never an empty reply.

The request hash is the SHA-256 of the canonical JSON of:
`provider` (constructor tag only), `wireApi` (effective, auth-independent), `model` (provider model
id), `host` (endpoint URL host), and `payload` (the provider-encoded request body from
`Codec.encodeRequest`). Headers, API keys, OAuth tokens, account ids, cloud credentials and
provider config fields are never part of it.
-/

namespace Leanllm

open Lean

/-- An LLM call as a plain function; matches wybe-agent's `AgentConfig.llmCaller`. -/
abbrev Caller := ModelConfig → List Message → IO (Except LLMError LLMResponse)

namespace Replay

/-! ## Canonical JSON -/

private def normalizeNumber (n : JsonNumber) : JsonNumber := Id.run do
  let mut m := n.mantissa
  let mut e := n.exponent
  while e > 0 && m % 10 == 0 do
    m := m / 10
    e := e - 1
  return ⟨m, e⟩

private partial def renderInto (acc : String) : Json → String
  | .null      => acc ++ "null"
  | .bool b    => acc ++ (if b then "true" else "false")
  | .num n     => acc ++ (Json.num (normalizeNumber n)).compress
  | .str s     => acc ++ (Json.str s).compress
  | .arr xs    => Id.run do
    let mut out := acc.push '['
    let mut first := true
    for x in xs do
      if !first then out := out.push ','
      first := false
      out := renderInto out x
    return out.push ']'
  | .obj kvs   => Id.run do
    let sorted := kvs.toList.toArray.qsort (fun a b => a.1 < b.1)
    let mut out := acc.push '{'
    let mut first := true
    for (k, v) in sorted do
      if !first then out := out.push ','
      first := false
      out := renderInto (out ++ (Json.str k).compress ++ ":") v
    return out.push '}'

/-- Canonical JSON text: object keys sorted recursively (by code point), no whitespace,
    numbers without trailing fractional zeros. -/
def canonicalString (j : Json) : String :=
  renderInto "" j

/-- Parse JSON text and return its canonical form. -/
def canonicalizeText (raw : String) : Except String String :=
  canonicalString <$> Json.parse raw

/-! ## Canonical request -/

/-- Constructor tag of a `Provider`. Deliberately drops all fields: Bedrock/Azure/Vertex configs can
    carry credentials, and routing details already show up in `host`, `model` or `payload`. -/
def providerTag : Provider → String
  | .direct        => "direct"
  | .openRouter _  => "openRouter"
  | .bedrock _     => "bedrock"
  | .azure _       => "azure"
  | .vertex _      => "vertex"
  | .copilot       => "copilot"
  | .antigravity   => "antigravity"
  | .grokBuild     => "grokBuild"
  | .kimiCode      => "kimiCode"
  | .zaiCoding     => "zaiCoding"
  | .alibabaCoding => "alibabaCoding"
  | .opencodeZen   => "opencodeZen"
  | .opencodeGo    => "opencodeGo"
  | .groq          => "groq"
  | .cerebras      => "cerebras"
  | .together      => "together"
  | .fireworks     => "fireworks"
  | .deepinfra     => "deepinfra"
  | .novita        => "novita"
  | .baseten       => "baseten"
  | .nebius        => "nebius"
  | .nvidiaNim     => "nvidiaNim"
  | .ollama        => "ollama"
  | .lmstudio      => "lmstudio"
  | .custom _      => "custom"

def wireApiTag : WireApi → String
  | .auto              => "auto"
  | .anthropicMessages => "anthropicMessages"
  | .openaiChat        => "openaiChat"
  | .openaiResponses   => "openaiResponses"
  | .bedrockConverse   => "bedrockConverse"
  | .bedrockInvoke     => "bedrockInvoke"
  | .geminiGenerate    => "geminiGenerate"

/-- Lowercased `host[:port]` of a URL, without scheme, userinfo, path or query. -/
def urlHost (url : String) : String :=
  let rest := match url.splitOn "://" with
    | _ :: r :: more => String.intercalate "://" (r :: more)
    | _              => url
  let authority := ((rest.splitOn "/").headD "").splitOn "?" |>.headD ""
  let authority := (authority.splitOn "#").headD ""
  ((authority.splitOn "@").getLastD "").toLower

/-- The canonical request object that is hashed (and stored in each record). -/
def canonicalRequest (cfg : ModelConfig) (messages : List Message) : Except LLMError Json := do
  let payload ← Codec.encodeRequest cfg messages
  return Json.mkObj [
    ("format",   "leanllm-replay/1"),
    ("provider", providerTag cfg.provider),
    ("wireApi",  wireApiTag cfg.effectiveWireApi),
    ("model",    cfg.effectiveModelId),
    ("host",     urlHost (Transport.resolveEndpointUrl cfg)),
    ("payload",  payload)
  ]

/-- Lowercase hex SHA-256 of the canonical JSON of a canonical request object. -/
def hashCanonical (canonical : Json) : String :=
  Sha256.hexOfString (canonicalString canonical)

/-- The request hash used as the record key. -/
def requestHash (cfg : ModelConfig) (messages : List Message) : Except LLMError String :=
  hashCanonical <$> canonicalRequest cfg messages

/-- Path of the record for `hash` inside `dir`. -/
def recordPath (dir : System.FilePath) (hash : String) : System.FilePath :=
  dir / s!"{hash}.json"

/-! ## Replay misses -/

/-- Prefix of the human-readable message of every replay-miss error. -/
def missMessagePrefix : String := "replay miss: "

private def missDetailPrefix : String := "replay-miss:"

/-- The error returned when no record exists for a request.

    `LLMError` has no replay constructor (adding one would break downstream matches), so a miss is
    `.invalidRequest (.malformedParameters "replay-miss:<hash>") "replay miss: ..."`. It is a
    non-retryable, request-side error, so retry loops for rate limits/network/provider errors do not
    spin on it. Use `isReplayMiss` to recognize it. -/
def missError (hash : String) (modelId : String) (dir : System.FilePath) : LLMError :=
  .invalidRequest (.malformedParameters s!"{missDetailPrefix}{hash}")
    s!"{missMessagePrefix}no recorded response for request {hash} (model {modelId}) in {dir}"

end Replay

/-- `some hash` if `err` is a replay miss, `none` otherwise. -/
def isReplayMiss : LLMError → Option String
  | .invalidRequest (.malformedParameters details) msg =>
    if msg.startsWith Replay.missMessagePrefix && details.startsWith "replay-miss:" then
      some ((details.splitOn "replay-miss:").getLastD "")
    else none
  | _ => none

namespace Replay

/-! ## Records -/

/-- One stored exchange: `<dir>/<hash>.json`. -/
structure Record where
  request    : Json
  response   : LLMResponse
  recordedAt : Int

def Record.toJson (r : Record) : Json :=
  Json.mkObj [
    ("requestHash", hashCanonical r.request),
    ("request",     r.request),
    ("response",    Lean.toJson r.response),
    ("recordedAt",  Lean.toJson r.recordedAt)
  ]

/-- Decode a record; unknown fields are ignored. -/
def Record.fromJson? (j : Json) : Except String Record := do
  return {
    request    := ← j.getObjVal? "request"
    response   := ← j.getObjValAs? LLMResponse "response"
    recordedAt := (j.getObjValAs? Int "recordedAt").toOption.getD 0
  }

/-- Write a record atomically (temp file + rename). IO errors propagate. -/
def writeRecord (dir : System.FilePath) (r : Record) : IO Unit := do
  IO.FS.createDirAll dir
  let path := recordPath dir (hashCanonical r.request)
  let tmp : System.FilePath := ⟨s!"{path}.tmp"⟩
  IO.FS.writeFile tmp (r.toJson.pretty ++ "\n")
  IO.FS.rename tmp path

/-- Look up the record for `(cfg, messages)`. Never touches the network. -/
def lookup (dir : System.FilePath) (cfg : ModelConfig) (messages : List Message) :
    IO (Except LLMError LLMResponse) := do
  match canonicalRequest cfg messages with
  | .error e => return .error e
  | .ok canonical =>
    let hash := hashCanonical canonical
    let path := recordPath dir hash
    if !(← path.pathExists) then
      return .error (missError hash cfg.effectiveModelId dir)
    let raw ← IO.FS.readFile path
    let corrupt (why : String) : LLMError :=
      .parse (.invalidJson raw why) s!"replay: corrupt record {path}: {why}"
    match Json.parse raw with
    | .error e => return .error (corrupt e)
    | .ok j =>
      match Record.fromJson? j with
      | .error e => return .error (corrupt e)
      | .ok r =>
        if canonicalString r.request != canonicalString canonical then
          return .error (corrupt s!"stored request does not match request {hash}")
        return .ok r.response

end Replay

namespace Recorder

/-- Call `inner`; on success store `<dir>/<hash>.json` and return the response unchanged.
    Errors from `inner` are returned as-is and not recorded. -/
def wrap (dir : System.FilePath) (inner : Caller) : Caller := fun cfg messages => do
  match Replay.canonicalRequest cfg messages with
  | .error e => return .error e
  | .ok canonical =>
    let res ← inner cfg messages
    if let .ok resp := res then
      Replay.writeRecord dir { request := canonical, response := resp, recordedAt := ← Auth.nowMs }
    return res

/-- Record live calls made with leanllm's `call`. -/
def live (dir : System.FilePath) : Caller :=
  wrap dir fun cfg messages => (call cfg messages).run

end Recorder

namespace Replayer

/-- Serve recorded responses from `dir` by request hash, with no network access.
    A missing record yields a replay-miss error (see `isReplayMiss`). -/
def caller (dir : System.FilePath) : Caller :=
  Replay.lookup dir

end Replayer

end Leanllm
