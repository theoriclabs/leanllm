namespace Leanllm

inductive GrokModel where
  | grok4_7
  | grok4_6
  | grok4_5
  | grok4_3
  | grokBuild0_1
  | codeFast1
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GrokModel

def toModelId : GrokModel → String
  | .grok4_7      => "grok-4.7"
  | .grok4_6      => "grok-4.6"
  | .grok4_5      => "grok-4.5"
  | .grok4_3      => "grok-4.3"
  | .grokBuild0_1 => "grok-build-0.1"
  | .codeFast1    => "grok-code-fast-1"
  | .custom id    => id

def toOpenRouterSlug : GrokModel → String
  | .custom id => if id.contains '/' then id else s!"x-ai/{id}"
  | m          => s!"x-ai/{m.toModelId}"

instance : ToString GrokModel where
  toString := toModelId

end GrokModel
end Leanllm
