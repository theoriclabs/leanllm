namespace Leanllm

inductive OpenAIModel where
  | gpt56Sol
  | gpt54
  | gpt4o
  | gpt4oMini
  | o3
  | o4Mini
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace OpenAIModel

def toModelId : OpenAIModel → String
  | .gpt56Sol   => "gpt-5.6-sol"
  | .gpt54      => "gpt-5.4"
  | .gpt4o      => "gpt-4o"
  | .gpt4oMini  => "gpt-4o-mini"
  | .o3         => "o3"
  | .o4Mini     => "o4-mini"
  | .custom id  => id

def toOpenRouterSlug : OpenAIModel → String
  | .custom id => if id.contains '/' then id else s!"openai/{id}"
  | m          => s!"openai/{m.toModelId}"

instance : ToString OpenAIModel where
  toString := toModelId

end OpenAIModel
end Leanllm
