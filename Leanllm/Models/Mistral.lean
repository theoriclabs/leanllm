namespace Leanllm

inductive MistralModel where
  | large4
  | medium35
  | small2603
  | devstral
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace MistralModel

def toModelId : MistralModel → String
  | .large4    => "mistral-large-4-0"
  | .medium35  => "mistral-medium-3-5"
  | .small2603 => "mistral-small-2603"
  | .devstral  => "devstral-2512"
  | .custom id => id

def toOpenRouterSlug : MistralModel → String
  | .custom id => if id.contains '/' then id else s!"mistralai/{id}"
  | m          => s!"mistralai/{m.toModelId}"

instance : ToString MistralModel where
  toString := toModelId

end MistralModel
end Leanllm
