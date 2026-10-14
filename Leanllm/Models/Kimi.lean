namespace Leanllm

inductive KimiModel where
  | k3
  | k2_7Code
  | k2_6
  | k2_5
  | k2Thinking
  | forCoding
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace KimiModel

def toModelId : KimiModel → String
  | .k3         => "kimi-k3"
  | .k2_7Code   => "kimi-k2.7-code"
  | .k2_6       => "kimi-k2.6"
  | .k2_5       => "kimi-k2.5"
  | .k2Thinking => "kimi-k2-thinking"
  | .forCoding  => "kimi-for-coding"
  | .custom id  => id

def toOpenRouterSlug : KimiModel → String
  | .forCoding => "moonshotai/kimi-k2.7-code"
  | .custom id => if id.contains '/' then id else s!"moonshotai/{id}"
  | m          => s!"moonshotai/{m.toModelId}"

instance : ToString KimiModel where
  toString := toModelId

end KimiModel
end Leanllm
