namespace Leanllm

inductive DeepSeekModel where
  | v41Flash
  | v4Pro
  | v4Flash
  | v32
  | chat
  | reasoner
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace DeepSeekModel

def toModelId : DeepSeekModel → String
  | .v41Flash  => "deepseek-v4.1-flash"
  | .v4Pro     => "deepseek-v4-pro"
  | .v4Flash   => "deepseek-v4-flash"
  | .v32       => "deepseek-v3.2"
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
