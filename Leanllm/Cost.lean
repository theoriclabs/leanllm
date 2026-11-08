namespace Leanllm

/-- Who pays for the tokens a response used.

    - `apiKey`: the call was billed per token to an API account (API key or explicit bearer token).
    - `subscription`: the call was covered by a subscription plan (Claude, ChatGPT/Codex, Gemini
      Code Assist, Antigravity, Copilot or Grok OAuth, or a coding-plan endpoint). The cost is then
      the list-price equivalent of the usage, not an amount actually billed. -/
inductive CostBasis where
  | apiKey
  | subscription
  deriving Repr, BEq, Inhabited, DecidableEq

/-- The cost of one response, in integer nano-USD (1 USD = 1_000_000_000 nano-USD).

    `rateVersion` is the `Pricing.priceTableVersion` the rates came from, so stored costs stay
    explainable after the price table changes. -/
structure Cost where
  nanoUsd     : Nat
  basis       : CostBasis
  rateVersion : String
  deriving Repr, BEq, Inhabited

end Leanllm
