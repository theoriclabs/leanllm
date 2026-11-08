import Leanllm
import LeanllmTests.Structured

/-!
Offline tests for usage decoding and cost (theoriclabs/leanllm#1). No network, no API keys:
every response below is a fixed JSON payload shaped like the provider's documented response
object (usage values chosen so each subset and tier is exercised).
-/

open Leanllm Lean

namespace LeanllmTests

structure Results where
  passed : Nat := 0
  failed : List String := []

abbrev TestM := StateT Results IO

def check (name : String) (ok : Bool) (detail : String := "") : TestM Unit := do
  if ok then
    modify fun r => { r with passed := r.passed + 1 }
  else
    IO.eprintln s!"FAIL: {name} {detail}"
    modify fun r => { r with failed := r.failed ++ [name] }

def checkEq [BEq α] [Repr α] (name : String) (actual expected : α) : TestM Unit :=
  check name (actual == expected) s!"\n  expected: {repr expected}\n  actual:   {repr actual}"

def parse! (s : String) : Json :=
  match Json.parse s with
  | .ok j => j
  | .error e => panic! s!"bad fixture JSON: {e}"

def apiKeyAuth : ResolvedAuth := .apiKey "test-key-not-real"
def claudeOAuthAuth : ResolvedAuth := .claudeOAuth { accessToken := "test-token-not-real" }

/-- Decode `json` with `cfg`'s wire API and attach the cost, as `callWithSession` does. -/
def decodeAndCost (cfg : ModelConfig) (json : String) (auth : ResolvedAuth := apiKeyAuth) :
    TestM (Option LLMResponse) := do
  match Codec.decodeResponseWithAuth cfg (parse! json) (some auth) with
  | .ok resp => return some (resp.withCost cfg auth)
  | .error e =>
    check s!"decode {cfg.model}" false (repr e |>.pretty)
    return none

def cost (nanoUsd : Nat) (basis : CostBasis := .apiKey) : Option Cost :=
  some { nanoUsd, basis, rateVersion := Pricing.priceTableVersion }

/-! ## Anthropic Messages -/

def anthropicJson : String := "{
  \"id\": \"msg_test\", \"type\": \"message\", \"role\": \"assistant\",
  \"content\": [{\"type\": \"text\", \"text\": \"hi\"}],
  \"stop_reason\": \"end_turn\",
  \"usage\": {
    \"input_tokens\": 50,
    \"cache_creation_input_tokens\": 1000,
    \"cache_read_input_tokens\": 2000,
    \"cache_creation\": {\"ephemeral_5m_input_tokens\": 400, \"ephemeral_1h_input_tokens\": 600},
    \"output_tokens\": 300,
    \"output_tokens_details\": {\"thinking_tokens\": 120}
  }
}"

def testAnthropic : TestM Unit := do
  let cfg : ModelConfig := { model := .claude .sonnet5_5 }
  let some resp ← decodeAndCost cfg anthropicJson | return
  -- input_tokens excludes cache reads/writes: 50 + 2000 + 1000.
  checkEq "anthropic usage" resp.usage (some {
    inputTokens := 3050, outputTokens := 300, cacheReadTokens := some 2000,
    cacheWriteTokens := some 1000, cacheWriteLongTtlTokens := some 600, reasoningTokens := some 120 })
  -- Sonnet 5.5: 50*2000 + 2000*100 + 400*2500 + 600*4000 + 300*10000 (thinking already in output)
  checkEq "anthropic cost" resp.cost (cost 6700000)
  -- Same usage via Claude subscription OAuth: list-price equivalent.
  let some sub ← decodeAndCost cfg anthropicJson claudeOAuthAuth | return
  checkEq "anthropic subscription cost" sub.cost (cost 6700000 .subscription)

def testAnthropicNoCache : TestM Unit := do
  let cfg : ModelConfig := { model := .claude .haiku4_5 }
  let some resp ← decodeAndCost cfg "{\"content\": [], \"usage\": {\"input_tokens\": 10, \"output_tokens\": 20}}" | return
  checkEq "anthropic plain usage" resp.usage (some { inputTokens := 10, outputTokens := 20 })
  checkEq "anthropic plain cost" resp.cost (cost (10 * 1000 + 20 * 5000))

/-- Claude Haiku 5.5 bills the whole request at the higher tier once the prompt (counting cache
    reads and writes) is over 100,000 tokens. -/
def testHaikuLongContext : TestM Unit := do
  let cfg : ModelConfig := { model := .claude .haiku5_5 }
  let json (input : Nat) := s!"\{\"content\": [], \"usage\": \{\"input_tokens\": {input}, \"cache_read_input_tokens\": 60000, \"output_tokens\": 100}}"
  let some short ← decodeAndCost cfg (json 40000) | return
  checkEq "haiku 5.5 short tier" short.cost (cost (40000 * 100 + 60000 * 10 + 100 * 500))
  let some long ← decodeAndCost cfg (json 40001) | return
  checkEq "haiku 5.5 long tier" long.cost (cost (40001 * 500 + 60000 * 50 + 100 * 2500))

/-! ## OpenAI Chat Completions -/

def chatJson : String := "{
  \"id\": \"chatcmpl-test\", \"object\": \"chat.completion\", \"model\": \"gpt-4o\",
  \"choices\": [{\"index\": 0, \"message\": {\"role\": \"assistant\", \"content\": \"hi\"}, \"finish_reason\": \"stop\"}],
  \"usage\": {
    \"prompt_tokens\": 1200, \"completion_tokens\": 500, \"total_tokens\": 1700,
    \"prompt_tokens_details\": {\"cached_tokens\": 1024, \"cache_write_tokens\": 0, \"audio_tokens\": 0},
    \"completion_tokens_details\": {\"reasoning_tokens\": 200, \"audio_tokens\": 0}
  }
}"

def testChat : TestM Unit := do
  let cfg : ModelConfig := { model := .openai .gpt4o }
  let some resp ← decodeAndCost cfg chatJson | return
  checkEq "chat usage" resp.usage (some {
    inputTokens := 1200, outputTokens := 500, cacheReadTokens := some 1024,
    cacheWriteTokens := some 0, reasoningTokens := some 200 })
  -- gpt-4o: (1200-1024)*2500 + 1024*1250 + 500*10000; cached and reasoning are subsets.
  checkEq "chat cost" resp.cost (cost 6720000)

/-! ## OpenAI Responses -/

/-- Usage numbers from the prompt-caching guide's worked example. -/
def responsesJson : String := "{
  \"id\": \"resp_test\", \"object\": \"response\", \"status\": \"completed\",
  \"output\": [{\"type\": \"message\", \"role\": \"assistant\", \"content\": [{\"type\": \"output_text\", \"text\": \"hi\"}]}],
  \"usage\": {
    \"input_tokens\": 12000,
    \"input_tokens_details\": {
      \"cached_tokens\": 4000, \"cache_write_tokens\": 7000,
      \"cache_write_tokens_details\": {\"ttl_30m_tokens\": 3000, \"ttl_12h_tokens\": 4000}
    },
    \"output_tokens\": 800,
    \"output_tokens_details\": {\"reasoning_tokens\": 300},
    \"total_tokens\": 12800
  }
}"

def testResponses : TestM Unit := do
  let cfg : ModelConfig := { model := .openai .gpt5_6Terra, wireApi := .openaiResponses }
  let some resp ← decodeAndCost cfg responsesJson | return
  checkEq "responses usage" resp.usage (some {
    inputTokens := 12000, outputTokens := 800, cacheReadTokens := some 4000,
    cacheWriteTokens := some 7000, cacheWriteLongTtlTokens := some 4000, reasoningTokens := some 300 })
  -- gpt-5.6-terra: 1000*2000 + 4000*200 + 3000*2500 + 4000*5000 + 800*12000
  checkEq "responses cost" resp.cost (cost 39900000)
  -- The guide's input-only formula at its $1 input price gives $0.01515; ours at $2 is double.
  checkEq "responses input cost matches guide" (39900000 - 800 * 12000) (2 * 15150000)
  -- Codex OAuth SSE path decodes `response.completed` with the same decoder.
  checkEq "responses usage via SSE decoder" (Codec.decodeOpenAIResponsesUsage (parse! responsesJson)) resp.usage

/-- gpt-5.5 has no 12h write price, so 12h writes cannot be priced: no guessing. -/
def testResponsesUnpriceableWrite : TestM Unit := do
  let cfg : ModelConfig := { model := .openai .gpt5_5, wireApi := .openaiResponses }
  let some resp ← decodeAndCost cfg responsesJson | return
  checkEq "responses 12h write without rate" resp.cost none

def testResponsesLongContext : TestM Unit := do
  let cfg : ModelConfig := { model := .openai .gpt5_4, wireApi := .openaiResponses }
  let json := "{\"output\": [], \"usage\": {\"input_tokens\": 300000, \"input_tokens_details\": {\"cached_tokens\": 0}, \"output_tokens\": 10, \"output_tokens_details\": {\"reasoning_tokens\": 0}}}"
  let some resp ← decodeAndCost cfg json | return
  checkEq "gpt-5.4 long context cost" resp.cost (cost (300000 * 5000 + 10 * 22500))

/-! ## Gemini -/

def geminiJson : String := "{
  \"candidates\": [{\"content\": {\"role\": \"model\", \"parts\": [{\"text\": \"hi\"}]}, \"finishReason\": \"STOP\"}],
  \"usageMetadata\": {
    \"promptTokenCount\": 5000, \"cachedContentTokenCount\": 4000,
    \"candidatesTokenCount\": 300, \"thoughtsTokenCount\": 700, \"totalTokenCount\": 6000
  },
  \"responseId\": \"gemini-test\"
}"

def testGemini : TestM Unit := do
  let cfg : ModelConfig := { model := .gemini .flash3_8 }
  let some resp ← decodeAndCost cfg geminiJson | return
  -- thoughts are reported outside candidates: output = 300 + 700.
  checkEq "gemini usage" resp.usage (some {
    inputTokens := 5000, outputTokens := 1000, cacheReadTokens := some 4000, reasoningTokens := some 700 })
  -- gemini-3.8-flash: 1000*750 + 4000*75 + 1000*3750
  checkEq "gemini cost" resp.cost (cost 4800000)
  -- Code Assist envelope with tool-use prompt tokens, via Gemini OAuth (subscription).
  let wrapped := "{\"response\": {\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"x\"}]}}], \"usageMetadata\": {\"promptTokenCount\": 100, \"toolUsePromptTokenCount\": 50, \"candidatesTokenCount\": 10, \"totalTokenCount\": 160}}}"
  let some w ← decodeAndCost cfg wrapped (.geminiOAuth { accessToken := "test-token-not-real" } "proj") | return
  checkEq "gemini tool-use usage" w.usage (some { inputTokens := 150, outputTokens := 10 })
  checkEq "gemini tool-use cost" w.cost (cost (150 * 750 + 10 * 3750) .subscription)

def testGeminiLongContext : TestM Unit := do
  let cfg : ModelConfig := { model := .gemini .pro2_5 }
  let json := "{\"candidates\": [{\"content\": {\"parts\": []}}], \"usageMetadata\": {\"promptTokenCount\": 200001, \"candidatesTokenCount\": 1}}"
  let some resp ← decodeAndCost cfg json | return
  checkEq "gemini 2.5 pro long context cost" resp.cost (cost (200001 * 2500 + 15000))

/-! ## Bedrock Converse -/

def testBedrockConverse : TestM Unit := do
  let cfg : ModelConfig := { model := .claude .sonnet4_5, provider := .bedrock {} }
  let json := "{\"output\": {\"message\": {\"role\": \"assistant\", \"content\": [{\"text\": \"hi\"}]}}, \"stopReason\": \"end_turn\",
    \"usage\": {\"inputTokens\": 100, \"outputTokens\": 20, \"totalTokens\": 620, \"cacheReadInputTokens\": 200, \"cacheWriteInputTokens\": 300,
    \"cacheDetails\": [{\"ttl\": \"1h\", \"inputTokens\": 250}, {\"ttl\": \"5m\", \"inputTokens\": 50}]}}"
  let some resp ← decodeAndCost cfg json | return
  checkEq "bedrock usage" resp.usage (some {
    inputTokens := 600, outputTokens := 20, cacheReadTokens := some 200,
    cacheWriteTokens := some 300, cacheWriteLongTtlTokens := some 250 })
  -- Bedrock has its own price list, not confirmed here.
  checkEq "bedrock cost unknown" resp.cost none

/-! ## No usage, no rate -/

def testMissingUsage : TestM Unit := do
  let cfg : ModelConfig := { model := .claude .sonnet5_5 }
  let some resp ← decodeAndCost cfg "{\"content\": [{\"type\": \"text\", \"text\": \"hi\"}]}" | return
  checkEq "missing usage" resp.usage none
  checkEq "missing usage gives no cost" resp.cost none
  -- The Codex SSE path builds a response with `usage := none` when no `response.completed` arrives.
  let bare : LLMResponse := { id := "", model := .openai .gpt5_5, message := { role := .assistant, content := [] }, stopReason := .endTurn }
  checkEq "SSE without completed event" (bare.withCost { model := .openai .gpt5_5 } (.openaiOAuth { accessToken := "t" })).cost none

def testNoRate : TestM Unit := do
  let some r1 ← decodeAndCost { model := .openai .o4Mini } chatJson | return
  checkEq "unlisted model gives no cost" r1.cost none
  let some r2 ← decodeAndCost { model := .openai (.custom "my-finetune") } chatJson | return
  checkEq "custom model gives no cost" r2.cost none
  let some r3 ← decodeAndCost { model := .openai .gpt4o, provider := .openRouter {} } chatJson | return
  checkEq "reseller gives no cost" r3.cost none

/-- OpenAI-compatible vendors share the Chat Completions decoder; spot-check their tables. -/
def testOtherVendors : TestM Unit := do
  let some glm ← decodeAndCost { model := .glm .glm5_3, provider := .zaiCoding } chatJson | return
  -- Z.AI coding plan: list-price equivalent, (1200-1024)*1400 + 1024*260 + 500*4400
  checkEq "glm-5.3 coding plan cost" glm.cost (cost (176 * 1400 + 1024 * 260 + 500 * 4400) .subscription)
  let some grok ← decodeAndCost { model := .grok .grok4_7 } chatJson | return
  checkEq "grok-4.7 cost" grok.cost (cost (176 * 2000 + 1024 * 500 + 500 * 6000))
  -- xAI: a prompt that reaches 200k tokens is billed at the higher tier.
  let big := "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"x\"}, \"finish_reason\": \"stop\"}], \"usage\": {\"prompt_tokens\": 200000, \"completion_tokens\": 1}}"
  let some grokLong ← decodeAndCost { model := .grok .grok4_7 } big | return
  checkEq "grok-4.7 long context cost" grokLong.cost (cost (200000 * 4000 + 12000))
  let some ds ← decodeAndCost { model := .deepseek .v4Pro } chatJson | return
  checkEq "deepseek time-of-day pricing gives no cost" ds.cost none

def testBasis : TestM Unit := do
  checkEq "basis apiKey" (Pricing.costBasis .direct apiKeyAuth) .apiKey
  checkEq "basis bearer" (Pricing.costBasis .direct (.bearer "t")) .apiKey
  checkEq "basis coding plan" (Pricing.costBasis .zaiCoding apiKeyAuth) .subscription
  checkEq "basis copilot" (Pricing.costBasis .copilot (.copilotBearer "t")) .subscription
  checkEq "basis claude oauth" (Pricing.costBasis .direct claudeOAuthAuth) .subscription

def all : TestM Unit := do
  testAnthropic
  testAnthropicNoCache
  testHaikuLongContext
  testChat
  testResponses
  testResponsesUnpriceableWrite
  testResponsesLongContext
  testGemini
  testGeminiLongContext
  testBedrockConverse
  testMissingUsage
  testNoRate
  testOtherVendors
  testBasis

end LeanllmTests

def main : IO UInt32 := do
  let ((), r) ← LeanllmTests.all.run {}
  let (sPassed, sFailed) ← LeanllmTests.Structured.run
  let passed := r.passed + sPassed
  let failed := r.failed ++ sFailed
  IO.println s!"leanllm-tests: {passed} passed, {failed.length} failed"
  return if failed.isEmpty then 0 else 1
