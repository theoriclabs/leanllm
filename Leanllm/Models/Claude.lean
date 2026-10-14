namespace Leanllm

inductive ClaudeModel where
  | opus5_5
  | sonnet5_5
  | haiku5_5
  | fable5_1
  | opus5
  | sonnet5
  | opus4_5
  | sonnet4_5
  | haiku4_5
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace ClaudeModel

def toModelId : ClaudeModel → String
  | .opus5_5   => "claude-opus-5-5"
  | .sonnet5_5 => "claude-sonnet-5-5"
  | .haiku5_5  => "claude-haiku-5-5"
  | .fable5_1  => "claude-fable-5-1"
  | .opus5     => "claude-opus-5"
  | .sonnet5   => "claude-sonnet-5"
  | .opus4_5   => "claude-opus-4-5-20251101"
  | .sonnet4_5 => "claude-sonnet-4-5-20250929"
  | .haiku4_5  => "claude-haiku-4-5-20251001"
  | .custom id => id

def toOpenRouterSlug : ClaudeModel → String
  | .opus5_5   => "anthropic/claude-opus-5.5"
  | .sonnet5_5 => "anthropic/claude-sonnet-5.5"
  | .haiku5_5  => "anthropic/claude-haiku-5.5"
  | .fable5_1  => "anthropic/claude-fable-5.1"
  | .opus5     => "anthropic/claude-opus-5"
  | .sonnet5   => "anthropic/claude-sonnet-5"
  | .opus4_5   => "anthropic/claude-opus-4.5"
  | .sonnet4_5 => "anthropic/claude-sonnet-4.5"
  | .haiku4_5  => "anthropic/claude-haiku-4.5"
  | .custom id => if id.contains '/' then id else s!"anthropic/{id}"

instance : ToString ClaudeModel where
  toString := toModelId

end ClaudeModel
end Leanllm
