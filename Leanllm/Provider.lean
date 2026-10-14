import Leanllm.Models.Claude
import Leanllm.Models.OpenAI
import Leanllm.Models.Gemini
import Leanllm.Models.DeepSeek
import Leanllm.Models.Kimi
import Leanllm.Models.Glm
import Leanllm.Models.Qwen
import Leanllm.Models.MiniMax
import Leanllm.Models.Grok
import Leanllm.Models.Mistral

namespace Leanllm

/-- Top-level sum over model families. -/
inductive Model where
  | claude   (m : ClaudeModel)
  | openai   (m : OpenAIModel)
  | gemini   (m : GeminiModel)
  | deepseek (m : DeepSeekModel)
  | kimi     (m : KimiModel)
  | glm      (m : GlmModel)
  | qwen     (m : QwenModel)
  | minimax  (m : MiniMaxModel)
  | grok     (m : GrokModel)
  | mistral  (m : MistralModel)
  deriving Repr, BEq, Inhabited

namespace Model

def toModelId : Model → String
  | .claude m   => m.toModelId
  | .openai m   => m.toModelId
  | .gemini m   => m.toModelId
  | .deepseek m => m.toModelId
  | .kimi m     => m.toModelId
  | .glm m      => m.toModelId
  | .qwen m     => m.toModelId
  | .minimax m  => m.toModelId
  | .grok m     => m.toModelId
  | .mistral m  => m.toModelId

def toOpenRouterSlug : Model → String
  | .claude m   => m.toOpenRouterSlug
  | .openai m   => m.toOpenRouterSlug
  | .gemini m   => m.toOpenRouterSlug
  | .deepseek m => m.toOpenRouterSlug
  | .kimi m     => m.toOpenRouterSlug
  | .glm m      => m.toOpenRouterSlug
  | .qwen m     => m.toOpenRouterSlug
  | .minimax m  => m.toOpenRouterSlug
  | .grok m     => m.toOpenRouterSlug
  | .mistral m  => m.toOpenRouterSlug

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
  | openRouter    (cfg : OpenRouterConfig := {})
  | bedrock       (cfg : BedrockConfig    := {})
  | azure         (cfg : AzureConfig)
  | vertex        (cfg : VertexConfig)
  | copilot                                -- GitHub Copilot subscription (api.githubcopilot.com)
  | antigravity                            -- Google Antigravity subscription (daily-cloudcode-pa.googleapis.com)
  | grokBuild                              -- xAI Grok CLI subscription (cli-chat-proxy.grok.com)
  | kimiCode                               -- Kimi Code subscription / coding agent endpoint (api.kimi.com/coding/v1)
  | zaiCoding                              -- Z.AI Coding Plan (api.z.ai/api/coding/paas/v4)
  | alibabaCoding                          -- Alibaba Cloud Bailian Coding Plan (coding-intl.dashscope.aliyuncs.com/v1)
  | opencodeZen                            -- OpenCode Zen (opencode.ai/zen/v1)
  | opencodeGo                             -- OpenCode Go (opencode.ai/zen/go/v1)
  | groq                                   -- Groq Cloud (api.groq.com/openai/v1)
  | cerebras                               -- Cerebras Inference (api.cerebras.ai/v1)
  | together                               -- Together AI (api.together.xyz/v1)
  | fireworks                              -- Fireworks AI (api.fireworks.ai/inference/v1)
  | deepinfra                              -- DeepInfra (api.deepinfra.com/v1/openai)
  | novita                                 -- Novita AI (api.novita.ai/openai)
  | baseten                                -- Baseten (inference.baseten.co/v1)
  | nebius                                 -- Nebius Token Factory (api.tokenfactory.nebius.com/v1)
  | nvidiaNim                              -- NVIDIA NIM (integrate.api.nvidia.com/v1)
  | ollama                                 -- Local Ollama (http://localhost:11434/v1)
  | lmstudio                               -- Local LM Studio (http://localhost:1234/v1)
  | custom        (baseUrl : String)
  deriving Repr, BEq, Inhabited

namespace Provider

/-- Resolve the default `WireApi` for a `(Provider, Model)` pair when `WireApi.auto` is selected. -/
def defaultWireApi (provider : Provider) (model : Model) : WireApi :=
  match provider, model with
  | .direct,        .claude _ => .anthropicMessages
  | .direct,        .gemini _ => .geminiGenerate
  | .direct,        _         => .openaiChat
  | .openRouter _,  _         => .openaiChat
  | .bedrock _,     _         => .bedrockConverse
  | .azure _,       _         => .openaiChat
  | .vertex _,      .claude _ => .anthropicMessages
  | .vertex _,      .gemini _ => .geminiGenerate
  | .vertex _,      _         => .openaiChat
  | .antigravity,   _         => .geminiGenerate
  | _,              _         => .openaiChat

/-- Whether a `Provider` supports hosting a given `Model` family. -/
def supportsModel : Provider → Model → Bool
  | .direct,        _         => true
  | .openRouter _,  _         => true
  | .bedrock _,     .claude _ => true
  | .bedrock _,     _         => false
  | .azure _,       .openai _ => true
  | .azure _,       _         => false
  | .vertex _,      .gemini _ => true
  | .vertex _,      .claude _ => true
  | .vertex _,      _         => false
  | .antigravity,   .gemini _ => true
  | .antigravity,   _         => false
  | .kimiCode,      .kimi _   => true
  | .kimiCode,      _         => false
  | .zaiCoding,     .glm _    => true
  | .zaiCoding,     _         => false
  | .alibabaCoding, .qwen _   => true
  | .alibabaCoding, _         => false
  | _,              _         => true

/-- Resolve the provider-specific model identifier string. -/
def resolveModelId (provider : Provider) (model : Model) : String :=
  match provider, model with
  | .direct, m => m.toModelId
  | .openRouter _, m => m.toOpenRouterSlug
  | .bedrock cfg, .claude m =>
    let prefixStr := if cfg.crossRegion then "us." else ""
    s!"{prefixStr}anthropic.{m.toModelId}-v1:0"
  | .bedrock _, m => m.toModelId
  | .azure cfg, m => cfg.deployment.getD m.toModelId
  | .vertex _, m => m.toModelId
  | .copilot, .claude m =>
    (m.toOpenRouterSlug.splitOn "/").getLastD m.toModelId
  | .copilot, m => m.toModelId
  | .together, m | .deepinfra, m | .novita, m | .baseten, m | .nebius, m | .nvidiaNim, m =>
    match m with
    | .deepseek _ | .kimi _ | .glm _ | .qwen _ => m.toOpenRouterSlug
    | _ => m.toModelId
  | _, m => m.toModelId

end Provider
end Leanllm
