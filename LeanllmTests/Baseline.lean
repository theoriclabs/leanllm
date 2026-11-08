import Leanllm

/-!
Requests encoded without a response format. `LeanllmTests.Golden` holds their encoding from
before structured output existed (generated on `main` at 76c79fe); the tests check they still match.
This file must keep compiling against that commit, so it uses no structured-output API.
-/

namespace LeanllmTests.Baseline

open Lean Leanllm

def weatherTool : ToolSpec := {
  name        := "get_weather"
  description := "Get the current weather for a city"
  parameters  := Json.mkObj [
    ("type", "object"),
    ("properties", Json.mkObj [("city", Json.mkObj [("type", "string")])]),
    ("required", toJson ["city"])
  ]
}

def messages : List Leanllm.Message := [
  Message.system "Be terse.",
  Message.user "Who wrote the Analytical Engine notes?"
]

def creds : OAuthCredentials := { accessToken := "test-token" }

/-- `(label, config, resolved auth)` for every wire API and auth envelope. -/
def cases : List (String × ModelConfig × Option ResolvedAuth) := [
  ("anthropic", { model := .claude .sonnet5_5, maxTokens := some 256, tools := [weatherTool] }, none),
  ("anthropic-oauth", { model := .claude .sonnet5_5 }, some (.claudeOAuth creds)),
  ("bedrock-invoke", { model := .claude .haiku4_5, provider := .bedrock {}, wireApi := .bedrockInvoke }, none),
  ("bedrock-converse", { model := .claude .haiku4_5, provider := .bedrock {}, tools := [weatherTool] }, none),
  ("openai-chat", { model := .openai .gpt5_4, temperature := some 0.5, tools := [weatherTool] }, none),
  ("openrouter-chat", { model := .deepseek .v4Flash, provider := .openRouter {} }, none),
  ("openai-responses", { model := .openai .gpt5_4, wireApi := .openaiResponses, maxTokens := some 128 }, none),
  ("chatgpt-oauth-responses", { model := .openai .gpt5_4 }, some (.openaiOAuth creds)),
  ("gemini", { model := .gemini .flash3_5, maxTokens := some 64 }, none),
  ("gemini-code-assist", { model := .gemini .flash3_5, maxTokens := some 64 }, some (.geminiOAuth creds "proj-1"))
]

def encode (cfg : ModelConfig) (auth : Option ResolvedAuth) : String :=
  match Codec.encodeRequestWithAuth cfg messages auth with
  | .ok j      => j.compress
  | .error err => s!"error: {repr err}"

end LeanllmTests.Baseline
