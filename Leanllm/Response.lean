import Leanllm.Provider
import Leanllm.Models
import Leanllm.Message
import Leanllm.Cost

namespace Leanllm

inductive StopReason where
  | endTurn
  | maxTokens
  | stopSequence (seq : Option String)
  | toolUse
  | contentFilter
  | other (raw : String)
  deriving Repr, BEq, Inhabited

/-- Token usage of one response, normalized to the same meaning on every wire API.

    Totals are inclusive and the optional fields are subsets of them, so
    `cacheReadTokens + cacheWriteTokens ≤ inputTokens`,
    `cacheWriteLongTtlTokens ≤ cacheWriteTokens` and `reasoningTokens ≤ outputTokens`.
    Never add a subset to its total again when charging (see `Leanllm.Pricing.costNanoUsd`).

    How each wire API maps onto these fields (documented in `Leanllm/Codec.lean`):
    - Anthropic Messages / Bedrock Invoke: `input_tokens` excludes cache reads and writes, so
      `inputTokens = input_tokens + cache_read_input_tokens + cache_creation_input_tokens`.
    - Bedrock Converse: same as Anthropic (`inputTokens + cacheReadInputTokens + cacheWriteInputTokens`).
    - OpenAI Chat Completions: `prompt_tokens` / `completion_tokens` are already inclusive.
    - OpenAI Responses: `input_tokens` / `output_tokens` are already inclusive.
    - Gemini: `inputTokens = promptTokenCount + toolUsePromptTokenCount` (prompt already includes
      cached content) and `outputTokens = candidatesTokenCount + thoughtsTokenCount` (thoughts are
      reported separately from candidates). -/
structure Usage where
  /-- All input (prompt) tokens processed, including cache reads and cache writes. -/
  inputTokens      : Nat
  /-- All generated tokens, including reasoning/thinking tokens. -/
  outputTokens     : Nat
  /-- Subset of `inputTokens` read from a prompt cache. -/
  cacheReadTokens  : Option Nat := none
  /-- Subset of `inputTokens` written to a prompt cache, for every TTL. -/
  cacheWriteTokens : Option Nat := none
  /-- Subset of `outputTokens` spent on reasoning/thinking (billed at the output rate). -/
  reasoningTokens  : Option Nat := none
  /-- Subset of `cacheWriteTokens` written with the long TTL (Anthropic/Bedrock 1h,
      OpenAI Responses 12h); the remainder used the default TTL (5m / 30m). -/
  cacheWriteLongTtlTokens : Option Nat := none
  deriving Repr, BEq, Inhabited

structure LLMResponse where
  id         : String
  model      : Model
  message    : Message
  stopReason : StopReason
  usage      : Option Usage := none
  /-- Cost of this response from `Leanllm.Pricing`; `none` when usage or the model's rate is unknown. -/
  cost       : Option Cost := none
  deriving Repr, BEq, Inhabited

namespace LLMResponse

/-- Extract concatenated text from the assistant's response message. -/
def text (resp : LLMResponse) : String :=
  let texts := resp.message.content.filterMap fun
    | .text t => some t
    | _       => none
  String.intercalate "\n" texts

/-- Extract all `ToolCall`s emitted by the assistant in this response. -/
def toolCalls (resp : LLMResponse) : List ToolCall :=
  resp.message.content.filterMap fun
    | .toolCall tc => some tc
    | _            => none

/-- Extract any thinking/reasoning trace blocks from the response. -/
def thinking (resp : LLMResponse) : List String :=
  resp.message.content.filterMap fun
    | .thinking t _ => some t
    | _             => none

end LLMResponse
end Leanllm
