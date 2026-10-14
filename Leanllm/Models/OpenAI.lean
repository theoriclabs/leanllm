namespace Leanllm

inductive OpenAIModel where
  | gpt4o
  | gpt4oMini
  | o3
  | o4Mini
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace OpenAIModel

def toModelId : OpenAIModel → String
  | .gpt4o      => "gpt-4o"
  | .gpt4oMini  => "gpt-4o-mini"
  | .o3         => "o3"
  | .o4Mini     => "o4-mini"
  | .custom id  => id

instance : ToString OpenAIModel where
  toString := toModelId

end OpenAIModel
end Leanllm
