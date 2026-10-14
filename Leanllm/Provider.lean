import Leanllm.Models.Claude
import Leanllm.Models.OpenAI
import Leanllm.Models.Gemini

namespace Leanllm

/-- Top-level sum over model families. -/
inductive Model where
  | claude (m : ClaudeModel)
  | openai (m : OpenAIModel)
  | gemini (m : GeminiModel)
  deriving Repr, BEq, Inhabited

namespace Model

def toModelId : Model → String
  | .claude m => m.toModelId
  | .openai m => m.toModelId
  | .gemini m => m.toModelId

instance : ToString Model where
  toString := toModelId

end Model

/-- Which JSON schema / wire dialect goes over the wire. -/
inductive WireApi where
  | auto               -- Use the provider's default dialect for the given model
  | anthropicMessages  -- Anthropic /v1/messages
  | openaiChat         -- OpenAI /v1/chat/completions
  | openaiResponses    -- OpenAI /v1/responses
  | bedrockConverse    -- AWS Bedrock /model/{id}/converse
  | bedrockInvoke      -- AWS Bedrock /model/{id}/invoke (native model payload)
  | geminiGenerate     -- Google /v1beta/models/{id}:generateContent
  deriving Repr, BEq, Inhabited

/-- Routing sort order for OpenRouter. -/
inductive ProviderSort where
  | price
  | throughput
  | latency
  deriving Repr, BEq, Inhabited

structure OpenRouterConfig where
  siteUrl        : Option String       := none
  appName        : Option String       := none
  fallbacks      : List Model          := []
  order          : List String         := []
  allowFallbacks : Bool                := true
  sort           : Option ProviderSort := none
  deriving Repr, BEq, Inhabited

structure BedrockConfig where
  region          : String        := "us-east-1"
  accessKeyId     : Option String := none
  secretAccessKey : Option String := none
  sessionToken    : Option String := none
  profile         : Option String := none
  crossRegion     : Bool          := true  -- e.g. "us.anthropic.claude-..."
  deriving Repr, BEq, Inhabited

structure AzureConfig where
  endpoint   : String
  deployment : Option String := none
  apiVersion : String        := "2024-10-21"
  deriving Repr, BEq, Inhabited

structure VertexConfig where
  projectId : String
  location  : String := "us-central1"
  deriving Repr, BEq, Inhabited

/-- Where the model is hosted and how requests are authenticated/routed. -/
inductive Provider where
  | direct
  | openRouter  (cfg : OpenRouterConfig := {})
  | bedrock     (cfg : BedrockConfig    := {})
  | azure       (cfg : AzureConfig)
  | vertex      (cfg : VertexConfig)
  | copilot                                -- GitHub Copilot subscription (api.githubcopilot.com)
  | antigravity                            -- Google Antigravity subscription (daily-cloudcode-pa.googleapis.com)
  | grokBuild                              -- xAI Grok CLI subscription (cli-chat-proxy.grok.com)
  | custom      (baseUrl : String)
  deriving Repr, BEq, Inhabited

namespace Provider

/-- Resolve the default `WireApi` for a `(Provider, Model)` pair when `WireApi.auto` is selected. -/
def defaultWireApi (provider : Provider) (model : Model) : WireApi :=
  match provider, model with
  | .direct,        .claude _ => .anthropicMessages
  | .direct,        .openai _ => .openaiChat
  | .direct,        .gemini _ => .geminiGenerate
  | .openRouter _,  _         => .openaiChat
  | .bedrock _,     _         => .bedrockConverse
  | .azure _,       _         => .openaiChat
  | .vertex _,      .claude _ => .anthropicMessages
  | .vertex _,      .gemini _ => .geminiGenerate
  | .vertex _,      .openai _ => .openaiChat
  | .copilot,       _         => .openaiChat
  | .antigravity,   _         => .geminiGenerate
  | .grokBuild,     _         => .openaiChat
  | .custom _,      _         => .openaiChat

/-- Whether a `Provider` supports hosting a given `Model` family. -/
def supportsModel : Provider → Model → Bool
  | .direct,       _         => true
  | .openRouter _, _         => true
  | .bedrock _,    .claude _ => true
  | .bedrock _,    _         => false
  | .azure _,      .openai _ => true
  | .azure _,      _         => false
  | .vertex _,     .gemini _ => true
  | .vertex _,     .claude _ => true
  | .vertex _,     .openai _ => false
  | .copilot,      _         => true
  | .antigravity,  .gemini _ => true
  | .antigravity,  _         => false
  | .grokBuild,    _         => true
  | .custom _,     _         => true

/-- Resolve the provider-specific model identifier string. -/
def resolveModelId (provider : Provider) (model : Model) : String :=
  match provider, model with
  | .direct, m => m.toModelId
  | .openRouter _, .claude m => m.toOpenRouterSlug
  | .openRouter _, .openai m => m.toOpenRouterSlug
  | .openRouter _, .gemini m => m.toOpenRouterSlug
  | .bedrock cfg, .claude m =>
    let prefixStr := if cfg.crossRegion then "us." else ""
    s!"{prefixStr}anthropic.{m.toModelId}-v1:0"
  | .bedrock _, m => m.toModelId
  | .azure cfg, m => cfg.deployment.getD m.toModelId
  | .vertex _, m => m.toModelId
  | .copilot, .claude m =>
    -- Copilot uses dot-separated version numbers for Claude models (e.g. claude-sonnet-4.5)
    (m.toOpenRouterSlug.splitOn "/").getLastD m.toModelId
  | .copilot, m => m.toModelId
  | .antigravity, m => m.toModelId
  | .grokBuild, m => m.toModelId
  | .custom _, m => m.toModelId

end Provider
end Leanllm
