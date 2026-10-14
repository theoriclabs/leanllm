namespace Leanllm

inductive GlmModel where
  | glm53Prime
  | glm53
  | glm53Flash
  | glm52
  | glm51
  | glm5
  | glm47
  | glm47Flash
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GlmModel

def toModelId : GlmModel → String
  | .glm53Prime => "glm-5.3-prime"
  | .glm53      => "glm-5.3"
  | .glm53Flash => "glm-5.3-flash"
  | .glm52      => "glm-5.2"
  | .glm51      => "glm-5.1"
  | .glm5       => "glm-5"
  | .glm47      => "glm-4.7"
  | .glm47Flash => "glm-4.7-flash"
  | .custom id  => id

def toOpenRouterSlug : GlmModel → String
  | .custom id => if id.contains '/' then id else s!"z-ai/{id}"
  | m          => s!"z-ai/{m.toModelId}"

instance : ToString GlmModel where
  toString := toModelId

end GlmModel
end Leanllm
