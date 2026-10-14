namespace Leanllm

inductive OpenAIModel where
  | gpt6Astra
  | gpt5_6Sol
  | gpt5_6Terra
  | gpt5_6Luna
  | gpt5_5
  | gpt5_4
  | gpt4o
  | gpt4oMini
  | o3
  | o4Mini
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace OpenAIModel

def toModelId : OpenAIModel → String
  | .gpt6Astra   => "gpt-6-astra"
  | .gpt5_6Sol   => "gpt-5.6-sol"
  | .gpt5_6Terra => "gpt-5.6-terra"
  | .gpt5_6Luna  => "gpt-5.6-luna"
  | .gpt5_5      => "gpt-5.5"
  | .gpt5_4      => "gpt-5.4"
  | .gpt4o       => "gpt-4o"
  | .gpt4oMini   => "gpt-4o-mini"
  | .o3          => "o3"
  | .o4Mini      => "o4-mini"
  | .custom id   => id

def toOpenRouterSlug : OpenAIModel → String
  | .custom id => if id.contains '/' then id else s!"openai/{id}"
  | m          => s!"openai/{m.toModelId}"

instance : ToString OpenAIModel where
  toString := toModelId

end OpenAIModel
end Leanllm
