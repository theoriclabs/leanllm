namespace Leanllm

inductive MiniMaxModel where
  | m3
  | m27
  | m25
  | m2
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace MiniMaxModel

def toModelId : MiniMaxModel → String
  | .m3        => "minimax-m3"
  | .m27       => "minimax-m2.7"
  | .m25       => "minimax-m2.5"
  | .m2        => "minimax-m2"
  | .custom id => id

def toOpenRouterSlug : MiniMaxModel → String
  | .custom id => if id.contains '/' then id else s!"minimax/{id}"
  | m          => s!"minimax/{m.toModelId}"

instance : ToString MiniMaxModel where
  toString := toModelId

end MiniMaxModel
end Leanllm
