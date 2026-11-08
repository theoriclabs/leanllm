import Leanllm.Provider
import Leanllm.Models
import Leanllm.Auth
import Leanllm.Cost
import Leanllm.Response

/-!
# Pricing

One versioned price table for every `Model`, and the cost of a response computed from it.

Rules:
- Rates are integer nano-USD per token (`$1 / MTok` = `1000` nano-USD per token). No `Float`.
- Every rate cites the vendor's pricing page and the date it was read. A model whose price could
  not be confirmed on the vendor's page has no rate (`none`): no guessing, no family defaults.
- Rates are first-party list prices (standard tier, global/default region). They do not cover
  batch, flex, fast/priority modes, data-residency uplifts, per-request tool fees (search,
  grounding) or cache storage per hour. leanllm sends none of those options itself.
- `cost = none` whenever usage is missing or no rate is known. A cost is never estimated.
-/

namespace Leanllm

/-- Per-token rates for one context tier, in nano-USD per token. -/
structure TokenRates where
  /-- Uncached input tokens. -/
  input         : Nat
  /-- Output tokens, including reasoning/thinking tokens (every provider here bills them as output). -/
  output        : Nat
  /-- Input tokens read from the prompt cache. -/
  cacheRead     : Nat
  /-- Input tokens written to the cache with the default TTL (Anthropic 5m, OpenAI 30m).
      Equal to `input` where the provider charges no cache-write premium. -/
  cacheWrite    : Nat
  /-- Input tokens written with the long TTL (Anthropic 1h, OpenAI 12h); `none` if the model has no
      long-TTL write price. -/
  cacheWriteLongTtl : Option Nat := none
  deriving Repr, BEq, Inhabited

/-- Rates for one model. `longContext = some (threshold, rates)` means a request whose
    `Usage.inputTokens` (all input tokens, including cache reads and writes) exceeds `threshold`
    is billed entirely at `rates`. -/
structure ModelRate extends TokenRates where
  longContext : Option (Nat × TokenRates) := none
  deriving Repr, BEq, Inhabited

namespace Pricing

/-- Version of the price table: the date the prices below were read. -/
def priceTableVersion : String := "2026-11-08"

/-! ## Anthropic (Claude API, first party)

Source: https://platform.claude.com/docs/en/about-claude/pricing (Model pricing table and
"Long context pricing"), read 2026-11-08. Columns: base input / 5m cache write / 1h cache write /
cache hit / output, USD per MTok. Thinking tokens are billed as output tokens. -/

def claudeRate : ClaudeModel → Option ModelRate
  -- Claude Opus 5.5: $4 / $5 / $8 / $0.20 / $20
  | .opus5_5   => some { input := 4000, cacheWrite := 5000, cacheWriteLongTtl := some 8000, cacheRead := 200, output := 20000 }
  -- Claude Sonnet 5.5: $2 / $2.50 / $4 / $0.10 / $10
  | .sonnet5_5 => some { input := 2000, cacheWrite := 2500, cacheWriteLongTtl := some 4000, cacheRead := 100, output := 10000 }
  -- Claude Haiku 5.5, prompts up to 100,000 tokens: $0.10 / $0.125 / $0.20 / $0.01 / $0.50;
  -- prompts over 100,000 tokens (counting cache reads and writes): $0.50 / $0.625 / $1 / $0.05 / $2.50
  | .haiku5_5  => some {
      input := 100, cacheWrite := 125, cacheWriteLongTtl := some 200, cacheRead := 10, output := 500
      longContext := some (100000,
        { input := 500, cacheWrite := 625, cacheWriteLongTtl := some 1000, cacheRead := 50, output := 2500 }) }
  -- Claude Fable 5.1: $10 / $12.50 / $20 / $0.25 / $50
  | .fable5_1  => some { input := 10000, cacheWrite := 12500, cacheWriteLongTtl := some 20000, cacheRead := 250, output := 50000 }
  -- Claude Opus 5: $5 / $6.25 / $10 / $0.50 / $25
  | .opus5     => some { input := 5000, cacheWrite := 6250, cacheWriteLongTtl := some 10000, cacheRead := 500, output := 25000 }
  -- Claude Sonnet 5: $2 / $2.50 / $4 / $0.20 / $10
  | .sonnet5   => some { input := 2000, cacheWrite := 2500, cacheWriteLongTtl := some 4000, cacheRead := 200, output := 10000 }
  -- Claude Opus 4.5: $5 / $6.25 / $10 / $0.50 / $25
  | .opus4_5   => some { input := 5000, cacheWrite := 6250, cacheWriteLongTtl := some 10000, cacheRead := 500, output := 25000 }
  -- Claude Sonnet 4.5: $3 / $3.75 / $6 / $0.30 / $15
  | .sonnet4_5 => some { input := 3000, cacheWrite := 3750, cacheWriteLongTtl := some 6000, cacheRead := 300, output := 15000 }
  -- Claude Haiku 4.5: $1 / $1.25 / $2 / $0.10 / $5
  | .haiku4_5  => some { input := 1000, cacheWrite := 1250, cacheWriteLongTtl := some 2000, cacheRead := 100, output := 5000 }
  | .custom _  => none

/-! ## OpenAI (OpenAI API, first party)

Source: https://developers.openai.com/api/docs/pricing ("Standard pricing data"), read 2026-11-08.
"Short context: ≤272K input tokens. Long context: >272K input tokens."
Cache writes: for GPT-5.6 and later, 30m writes cost 1.25x and 12h writes 2.5x input (the
"30m write" / "12h write" columns); earlier models have "No additional cache-write charge"
(https://developers.openai.com/api/docs/guides/prompt-caching, read 2026-11-08), so their write
rate equals the input rate. Reasoning tokens are billed as output tokens.
GPT-5.6 Sol's listed price is promotional "until November 30, 2026". -/

private def openaiRates (input output cached write30m write12h : Nat) : TokenRates :=
  { input, output, cacheRead := cached, cacheWrite := write30m, cacheWriteLongTtl := some write12h }

private def openaiRatesNoWritePremium (input output cached : Nat) : TokenRates :=
  { input, output, cacheRead := cached, cacheWrite := input }

def openaiRate : OpenAIModel → Option ModelRate
  -- gpt-6-astra: $10 / $50 / $1.00 / $12.50 / $25.00; long $20 / $75 / $2.00 / $25 / $50
  | .gpt6Astra   => some { toTokenRates := openaiRates 10000 50000 1000 12500 25000
                           longContext := some (272000, openaiRates 20000 75000 2000 25000 50000) }
  -- gpt-5.6-sol: $4 / $20 / $0.40 / $5 / $10; long $8 / $30 / $0.80 / $10 / $20
  | .gpt5_6Sol   => some { toTokenRates := openaiRates 4000 20000 400 5000 10000
                           longContext := some (272000, openaiRates 8000 30000 800 10000 20000) }
  -- gpt-5.6-terra: $2 / $12 / $0.20 / $2.50 / $5; long $4 / $18 / $0.40 / $5 / $10
  | .gpt5_6Terra => some { toTokenRates := openaiRates 2000 12000 200 2500 5000
                           longContext := some (272000, openaiRates 4000 18000 400 5000 10000) }
  -- gpt-5.6-luna: $0.20 / $1.20 / $0.02 / $0.25 / $0.50; long $0.40 / $1.80 / $0.04 / $0.50 / $1.00
  | .gpt5_6Luna  => some { toTokenRates := openaiRates 200 1200 20 250 500
                           longContext := some (272000, openaiRates 400 1800 40 500 1000) }
  -- gpt-5.5: $5 / $30 / $0.50; long $10 / $45 / $1.00
  | .gpt5_5      => some { toTokenRates := openaiRatesNoWritePremium 5000 30000 500
                           longContext := some (272000, openaiRatesNoWritePremium 10000 45000 1000) }
  -- gpt-5.4: $2.50 / $15 / $0.25; long $5 / $22.50 / $0.50
  | .gpt5_4      => some { toTokenRates := openaiRatesNoWritePremium 2500 15000 250
                           longContext := some (272000, openaiRatesNoWritePremium 5000 22500 500) }
  -- gpt-4o: $2.50 / $10 / $1.25
  | .gpt4o       => some { toTokenRates := openaiRatesNoWritePremium 2500 10000 1250 }
  -- gpt-4o-mini: $0.15 / $0.60 / $0.075
  | .gpt4oMini   => some { toTokenRates := openaiRatesNoWritePremium 150 600 75 }
  -- o3: $2 / $8 / $0.50
  | .o3          => some { toTokenRates := openaiRatesNoWritePremium 2000 8000 500 }
  -- o4-mini is not listed on the pricing page.
  | .o4Mini      => none
  | .custom _    => none

/-! ## Google Gemini (Gemini Developer API, paid tier, Standard)

Source: https://ai.google.dev/gemini-api/docs/pricing (redirects to
https://aistudio.google.com/docs/pricing), read 2026-11-08. Columns: input / output (the page
says "Output price (including thinking tokens)") / context caching price, USD per MTok.
Gemini has no per-token cache-write charge (explicit cache storage is billed per hour, which is
not part of a response), so the write rate equals the input rate.
Scheduled changes: gemini-4-argon rises to $4 / $20 / $0.20 on 2026-12-01, gemini-3.8-flash to
$1.50 / $7.50 / $0.15 on 2027-01-01; gemini-2.5-pro and gemini-2.5-flash route to newer models
from 2026-12-01. The table must be re-versioned by then. -/

private def geminiRates (input output cached : Nat) : TokenRates :=
  { input, output, cacheRead := cached, cacheWrite := input }

def geminiRate : GeminiModel → Option ModelRate
  -- gemini-4-argon: $2.00 / $10.00 / $0.10 (through 2026-11-30)
  | .argon4   => some { toTokenRates := geminiRates 2000 10000 100 }
  -- gemini-3.8-flash: $0.75 / $3.75 / $0.075 (through 2026-12-31)
  | .flash3_8 => some { toTokenRates := geminiRates 750 3750 75 }
  -- gemini-3.5-flash is not listed on the pricing page (only gemini-3.5-flash-lite).
  | .flash3_5 => none
  -- gemini-2.5-pro: prompts <= 200k: $1.25 / $10.00 / $0.125; > 200k: $2.50 / $15.00 / $0.25
  | .pro2_5   => some { toTokenRates := geminiRates 1250 10000 125
                        longContext := some (200000, geminiRates 2500 15000 250) }
  -- gemini-2.5-flash: $0.30 (text / image / video) / $2.50 / $0.03. leanllm sends no audio parts,
  -- so the audio input price ($1.00) does not apply.
  | .flash2_5 => some { toTokenRates := geminiRates 300 2500 30 }
  | .custom _ => none

/-- First-party list-price rate of a model, independent of where it is hosted. -/
def listRate : Model → Option ModelRate
  | .claude m   => claudeRate m
  | .openai m   => openaiRate m
  | .gemini m   => geminiRate m
  | .deepseek _ => none
  | .kimi _     => none
  | .glm _      => none
  | .qwen _     => none
  | .minimax _  => none
  | .grok _     => none
  | .mistral _  => none

/-- Rate for a model served by a provider.

    - `.direct` uses the vendor's own list price.
    - Subscription endpoints that serve the vendor's models (`.copilot`, `.antigravity`,
      `.grokBuild`, `.kimiCode`, `.zaiCoding`, `.alibabaCoding`) also use the vendor list price:
      their cost is the list-price equivalent (`CostBasis.subscription`).
    - Resellers and clouds with their own price lists (OpenRouter, Bedrock, Azure, Vertex,
      inference hosts, OpenCode, local servers, custom URLs) have no confirmed rates: `none`. -/
def rateFor : Provider → Model → Option ModelRate
  | .direct, m        => listRate m
  | .copilot, m       => listRate m
  | .antigravity, m   => listRate m
  | .grokBuild, m     => listRate m
  | .kimiCode, m      => listRate m
  | .zaiCoding, m     => listRate m
  | .alibabaCoding, m => listRate m
  | _, _              => none

/-- Whether a call is paid per token or covered by a plan, from the provider and the resolved auth.
    Subscription OAuth logins and Copilot are plans; so are the coding-plan endpoints even though
    they authenticate with an API key. -/
def costBasis (provider : Provider) : ResolvedAuth → CostBasis
  | .apiKey _ =>
    match provider with
    | .kimiCode | .zaiCoding | .alibabaCoding | .opencodeGo => .subscription
    | _ => .apiKey
  | .bearer _ => .apiKey
  | .openaiOAuth _ | .claudeOAuth _ | .geminiOAuth _ _ | .antigravityOAuth _ _
  | .copilotBearer _ | .grokOAuth _ => .subscription

/-- Cost in nano-USD of `usage` at `rate`, or `none` if the usage needs a price the rate lacks
    (long-TTL cache writes on a model without a long-TTL write price).

    Cached and reasoning tokens are subsets (see `Usage`), so each input token is charged exactly
    once: ordinary input = `inputTokens - cacheReadTokens - cacheWriteTokens`; reasoning tokens are
    already inside `outputTokens` and are not charged again. -/
def costNanoUsd (rate : ModelRate) (usage : Usage) : Option Nat :=
  let r : TokenRates := match rate.longContext with
    | some (threshold, long) => if usage.inputTokens > threshold then long else rate.toTokenRates
    | none => rate.toTokenRates
  let cacheRead := usage.cacheReadTokens.getD 0
  let cacheWrite := usage.cacheWriteTokens.getD 0
  let longWrite := min (usage.cacheWriteLongTtlTokens.getD 0) cacheWrite
  let shortWrite := cacheWrite - longWrite
  let ordinaryInput := usage.inputTokens - cacheRead - cacheWrite
  let longWriteCost? : Option Nat :=
    if longWrite == 0 then some 0 else r.cacheWriteLongTtl.map (longWrite * ·)
  longWriteCost?.map fun longWriteCost =>
    ordinaryInput * r.input + cacheRead * r.cacheRead + shortWrite * r.cacheWrite
      + longWriteCost + usage.outputTokens * r.output

/-- The `Cost` of `usage` for `model` on `provider`, or `none` without a rate. -/
def costOf (provider : Provider) (model : Model) (basis : CostBasis) (usage : Usage) : Option Cost := do
  let rate ← rateFor provider model
  let nanoUsd ← costNanoUsd rate usage
  pure { nanoUsd, basis, rateVersion := priceTableVersion }

end Pricing

/-- Fill `resp.cost` from its usage, the configured provider and model, and the resolved auth.
    No usage or no rate leaves `cost = none`. -/
def LLMResponse.withCost (resp : LLMResponse) (cfg : ModelConfig) (auth : ResolvedAuth) : LLMResponse :=
  { resp with cost := resp.usage.bind (Pricing.costOf cfg.provider cfg.model (Pricing.costBasis cfg.provider auth)) }

end Leanllm
