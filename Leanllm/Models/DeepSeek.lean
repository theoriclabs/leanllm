namespace Leanllm

inductive DeepSeekModel where
  | v4_1Flash
  | v4Pro
  | v4Flash
  | v3_2
  | chat
  | reasoner
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace DeepSeekModel

def toModelId : DeepSeekModel → String
  | .v4_1Flash => "deepseek-v4.1-flash"
  | .v4Pro     => "deepseek-v4-pro"
  | .v4Flash   => "deepseek-v4-flash"
  | .v3_2      => "deepseek-v3.2"
  | .chat      => "deepseek-chat"
  | .reasoner  => "deepseek-reasoner"
  | .custom id => id

def toOpenRouterSlug : DeepSeekModel → String
  | .custom id => if id.contains '/' then id else s!"deepseek/{id}"
  | m          => s!"deepseek/{m.toModelId}"

instance : ToString DeepSeekModel where
  toString := toModelId

end DeepSeekModel
end Leanllm
