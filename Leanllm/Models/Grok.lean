namespace Leanllm

inductive GrokModel where
  | grok47
  | grok46
  | grok45
  | grok43
  | grokBuild01
  | codeFast1
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GrokModel

def toModelId : GrokModel → String
  | .grok47      => "grok-4.7"
  | .grok46      => "grok-4.6"
  | .grok45      => "grok-4.5"
  | .grok43      => "grok-4.3"
  | .grokBuild01 => "grok-build-0.1"
  | .codeFast1   => "grok-code-fast-1"
  | .custom id   => id

def toOpenRouterSlug : GrokModel → String
  | .custom id => if id.contains '/' then id else s!"x-ai/{id}"
  | m          => s!"x-ai/{m.toModelId}"

instance : ToString GrokModel where
  toString := toModelId

end GrokModel
end Leanllm
