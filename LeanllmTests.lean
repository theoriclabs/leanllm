import Leanllm

/-! Offline tests: no network, no API keys. Run with `lake test`. -/

open Leanllm
open Lean (Json ToJson FromJson toJson fromJson?)

/-- Compare call results (`Except` has no `BEq`). -/
def sameRes (a b : Except LLMError LLMResponse) : Bool :=
  match a, b with
  | .ok x,    .ok y    => x == y
  | .error e, .error f => e == f
  | _,        _        => false

structure Counts where
  passed : Nat := 0
  failed : Nat := 0

def check (ref : IO.Ref Counts) (label : String) (ok : Bool) (detail : String := "") : IO Unit := do
  if ok then
    ref.modify fun c => { c with passed := c.passed + 1 }
  else
    ref.modify fun c => { c with failed := c.failed + 1 }
    IO.eprintln s!"FAIL: {label}{if detail.isEmpty then "" else s!" ({detail})"}"

/-! ## SHA-256 -/

def shaTests (r : IO.Ref Counts) : IO Unit := do
  let vectors : List (String × String) := [
    ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
    ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"),
    ("The quick brown fox jumps over the lazy dog",
      "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"),
    ("".pushn 'a' 55, "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"),
    ("".pushn 'a' 64, "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"),
    ("".pushn 'a' 1000, "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"),
    ("héllo ✓", "5657cdef8a85a584e0e961e6f8247cf5d3f8ed21496ed6fdbcfd43a761e94245")
  ]
  for (input, expected) in vectors do
    let got := Sha256.hexOfString input
    check r s!"sha256 of {input.length}-char input" (got == expected) got

/-! ## Canonical JSON -/

def canonicalTests (r : IO.Ref Counts) : IO Unit := do
  let a := Replay.canonicalizeText "{\"b\":1,\"a\":{\"d\":[1,2],\"c\":\"x\"}}"
  let b := Replay.canonicalizeText "{ \"a\" : { \"c\":\"x\",\n \"d\":[ 1, 2 ] },\t\"b\": 1 }"
  check r "canonical: key order and whitespace ignored" (a.toOption == b.toOption && a.isOk)
  check r "canonical: exact text" (a.toOption == some "{\"a\":{\"c\":\"x\",\"d\":[1,2]},\"b\":1}")
    (toString (repr a.toOption))
  let built1 := Json.mkObj [("z", 1), ("y", Json.mkObj [("q", "é\"\n"), ("p", Json.null)])]
  let built2 := Json.mkObj [("y", Json.mkObj [("p", Json.null), ("q", "é\"\n")]), ("z", 1)]
  check r "canonical: constructed objects" (Replay.canonicalString built1 == Replay.canonicalString built2)
  check r "canonical: trailing fractional zeros" ((Replay.canonicalizeText "[1.50]").toOption == (Replay.canonicalizeText "[1.5]").toOption)

/-! ## JSON round-trips -/

def roundTrip [ToJson α] [FromJson α] [BEq α] (x : α) : Bool :=
  match fromJson? (toJson x) with
  | .ok y    => y == x
  | .error _ => false

def jsonTests (r : IO.Ref Counts) : IO Unit := do
  let parts : List ContentPart := [
    .text "hi",
    .image (.url "https://example.com/a.png"),
    .image (.base64 "image/png" "AAAA"),
    .toolCall { id := "c1", name := "get_weather", arguments := Json.mkObj [("city", "Paris")] },
    .toolResult { toolCallId := "c1", content := "sunny", isError := true },
    .thinking "hmm",
    .thinking "hmm" (some "sig")
  ]
  for p in parts do
    check r s!"round-trip ContentPart {repr p}" (roundTrip p)
  for role in [Role.system, .developer, .user, .assistant, .tool] do
    check r s!"round-trip Role {repr role}" (roundTrip role)
  let msg : Message := { role := .assistant, content := parts, name := some "bot" }
  check r "round-trip Message" (roundTrip msg)
  for sr in [StopReason.endTurn, .maxTokens, .stopSequence none, .stopSequence (some "END"), .toolUse,
             .contentFilter, .other "weird"] do
    check r s!"round-trip StopReason {repr sr}" (roundTrip sr)
  let u : Usage := { inputTokens := 10, outputTokens := 5, cacheReadTokens := some 3, reasoningTokens := some 1 }
  check r "round-trip Usage" (roundTrip u)
  let models : List Model := [.claude .sonnet5_5, .claude (.custom "claude-x"), .openai .gpt4oMini,
    .gemini .flash3_5, .kimi .forCoding, .glm (.custom "glm-z"), .mistral (.custom "m"),
    .deepseek (.custom "d"), .qwen (.custom "q"), .minimax (.custom "mm"), .grok (.custom "g")]
  for m in models do
    check r s!"round-trip Model {m}" (roundTrip m)
  -- An unknown (e.g. renamed) variant falls back to `.custom id`.
  let renamed := Json.mkObj [("family", "claude"), ("id", "claude-old"), ("variant", "noSuchModel")]
  check r "Model: unknown variant falls back to custom"
    ((fromJson? renamed : Except String Model).toOption == some (.claude (.custom "claude-old")))
  let resp : LLMResponse := { id := "r1", model := .claude .haiku5_5, message := msg,
                              stopReason := .toolUse, usage := some u }
  check r "round-trip LLMResponse" (roundTrip resp)
  check r "round-trip LLMResponse without usage" (roundTrip { resp with usage := none })

/-! ## Request hash -/

def baseCfg : ModelConfig := { model := .claude .sonnet5_5, maxTokens := some 64 }

def hashOf (cfg : ModelConfig) (msgs : List Message) : String :=
  (Replay.requestHash cfg msgs).toOption.getD "<error>"

def hashTests (r : IO.Ref Counts) : IO Unit := do
  let msgs := [Message.system "Be brief.", Message.user "Hello"]
  let h := hashOf baseCfg msgs
  check r "hash: 64 hex chars" (h.length == 64) h
  check r "hash: deterministic" (h == hashOf baseCfg msgs)
  check r "hash: excludes apiKey and auth mode"
    (h == hashOf { baseCfg with apiKey := some "sk-test-not-a-real-key", auth := .apiKey (some "sk-other") } msgs)
  check r "hash: excludes bearer token" (h == hashOf { baseCfg with auth := .bearer "tok" } msgs)
  let br1 : ModelConfig := { baseCfg with provider := .bedrock {} }
  let br2 : ModelConfig := { baseCfg with provider := .bedrock { accessKeyId := some "AKIA-TEST", secretAccessKey := some "s" } }
  check r "hash: excludes provider credentials" (hashOf br1 msgs == hashOf br2 msgs)
  check r "hash: provider matters" (h != hashOf br1 msgs)
  check r "hash: prompt matters" (h != hashOf baseCfg [Message.system "Be brief.", Message.user "Hello!"])
  check r "hash: model matters" (h != hashOf { baseCfg with model := .claude .haiku5_5 } msgs)
  check r "hash: parameters matter" (h != hashOf { baseCfg with temperature := some 0.5 } msgs)
  check r "hash: base URL host matters" (h != hashOf { baseCfg with baseUrl := some "http://localhost:8080" } msgs)
  check r "hash: tools matter"
    (h != hashOf { baseCfg with tools := [{ name := "t", description := "d" }] } msgs)
  check r "urlHost" (Replay.urlHost "https://user:pw@API.Example.com:8443/v1/x?key=1" == "api.example.com:8443")

/-! ## Record / replay -/

/-- A mocked provider: echoes the last user message; counts calls. -/
def mockInner (calls : IO.Ref Nat) : Caller := fun cfg msgs => do
  calls.modify (· + 1)
  let n ← calls.get
  let lastUser := (msgs.filter (·.role == .user)).getLast?.map (fun m =>
    String.intercalate "" (m.content.filterMap fun | .text t => some t | _ => none)) |>.getD ""
  return .ok {
    id := s!"mock-{n}", model := cfg.model
    message := { role := .assistant, content := [.text s!"echo: {lastUser}"] }
    stopReason := .endTurn, usage := some { inputTokens := n, outputTokens := 2 * n }
  }

def failingInner : Caller := fun _ _ =>
  return .error (.provider .overloaded "mock overloaded")

def replayTests (r : IO.Ref Counts) : IO Unit := IO.FS.withTempDir fun dir => do
  let calls ← IO.mkRef 0
  let recorder := Recorder.wrap dir (mockInner calls)
  let turn1 := [Message.system "Be brief.", Message.user "What is Lean?"]
  let resp1 ← recorder baseCfg turn1
  let turn2 := turn1 ++ [Message.assistant "A prover.", Message.user "And Lake?"]
  let resp2 ← recorder baseCfg turn2
  let cfg2 : ModelConfig := { model := .openai .gpt4oMini, temperature := some 0.2 }
  let resp3 ← recorder cfg2 [Message.user "Hi"]
  check r "record: inner called 3 times" ((← calls.get) == 3)
  let files := (← dir.readDir).filter (·.fileName.endsWith ".json")
  check r "record: 3 record files" (files.size == 3) s!"{files.size}"
  check r "record: file named by hash"
    (← (Replay.recordPath dir (hashOf baseCfg turn1)).pathExists)

  -- Replay with no inner caller at all: nothing but file reads.
  let replayer := Replayer.caller dir
  let rep1 ← replayer baseCfg turn1
  let rep2 ← replayer baseCfg turn2
  let rep3 ← replayer cfg2 [Message.user "Hi"]
  check r "replay: identical response 1" (sameRes rep1 resp1 && resp1.isOk)
  check r "replay: identical response 2" (sameRes rep2 resp2 && resp2.isOk)
  check r "replay: identical response 3" (sameRes rep3 resp3 && resp3.isOk)
  check r "replay: inner not called" ((← calls.get) == 3)
  -- Credentials do not affect lookup.
  let repKey ← replayer { baseCfg with apiKey := some "sk-test-not-a-real-key" } turn1
  check r "replay: apiKey does not change lookup" (sameRes repKey resp1)

  -- Change one prompt: a typed miss naming the new hash.
  let changed := [Message.system "Be brief.", Message.user "What is Lean 4?"]
  let newHash := hashOf baseCfg changed
  match ← replayer baseCfg changed with
  | .ok _ => check r "miss: changed prompt is an error" false "got a response"
  | .error err =>
    check r "miss: isReplayMiss names the new hash" (isReplayMiss err == some newHash) (toString (repr err))
    let msg := match err with | .invalidRequest _ m => m | _ => ""
    check r "miss: message names hash and model"
      ((msg.splitOn newHash).length > 1 && (msg.splitOn baseCfg.effectiveModelId).length > 1) msg
  check r "isReplayMiss: other errors" (isReplayMiss (.provider .overloaded "x") == none)
  check r "isReplayMiss: plain malformedParameters"
    (isReplayMiss (.invalidRequest (.malformedParameters "bad") "bad") == none)

  -- A record with unknown fields (top-level and inside the response) still loads.
  let path := Replay.recordPath dir (hashOf baseCfg turn1)
  match Json.parse (← IO.FS.readFile path) with
  | .error e => check r "forward-compat: parse record" false e
  | .ok j =>
    let resp := (j.getObjValD "response").setObjVal! "cost" (Json.mkObj [("totalUsd", toJson (0.001 : Float))])
    let j' := (j.setObjVal! "response" resp).setObjVal! "futureField" (toJson [1, 2, 3])
    IO.FS.writeFile path j'.pretty
    check r "forward-compat: extra unknown fields still load" (sameRes (← replayer baseCfg turn1) resp1)

  -- Failed inner calls are returned unchanged and not recorded.
  let failing := Recorder.wrap dir failingInner
  let errRes ← failing baseCfg changed
  check r "record: inner error passed through" (sameRes errRes (.error (.provider .overloaded "mock overloaded")))
  check r "record: inner error not recorded" (!(← (Replay.recordPath dir newHash).pathExists))

  -- A corrupt record is a parse error, not a miss and not a reply.
  IO.FS.writeFile (Replay.recordPath dir newHash) "{not json"
  match ← replayer baseCfg changed with
  | .error (.parse _ _) => check r "corrupt record: parse error" true
  | other => check r "corrupt record: parse error" false (toString (repr other))

def main : IO UInt32 := do
  let r ← IO.mkRef ({} : Counts)
  shaTests r
  canonicalTests r
  jsonTests r
  hashTests r
  replayTests r
  let c ← r.get
  IO.println s!"leanllm-tests: {c.passed} passed, {c.failed} failed"
  return if c.failed == 0 then 0 else 1
