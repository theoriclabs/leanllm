namespace Leanllm

inductive ClaudeModel where
  | opus4
  | sonnet4
  | haiku35
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace ClaudeModel

def toModelId : ClaudeModel → String
  | .opus4      => "claude-opus-4-20250514"
  | .sonnet4    => "claude-sonnet-4-20250514"
  | .haiku35    => "claude-3-5-haiku-latest"
  | .custom id  => id

instance : ToString ClaudeModel where
  toString := toModelId

end ClaudeModel
end Leanllm
