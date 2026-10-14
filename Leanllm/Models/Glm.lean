namespace Leanllm

inductive GlmModel where
  | glm5_3Prime
  | glm5_3
  | glm5_3Flash
  | glm5_2
  | glm5_1
  | glm5
  | glm4_7
  | glm4_7Flash
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GlmModel

def toModelId : GlmModel → String
  | .glm5_3Prime => "glm-5.3-prime"
  | .glm5_3      => "glm-5.3"
  | .glm5_3Flash => "glm-5.3-flash"
  | .glm5_2      => "glm-5.2"
  | .glm5_1      => "glm-5.1"
  | .glm5        => "glm-5"
  | .glm4_7      => "glm-4.7"
  | .glm4_7Flash => "glm-4.7-flash"
  | .custom id   => id

def toOpenRouterSlug : GlmModel → String
  | .custom id => if id.contains '/' then id else s!"z-ai/{id}"
  | m          => s!"z-ai/{m.toModelId}"

instance : ToString GlmModel where
  toString := toModelId

end GlmModel
end Leanllm
