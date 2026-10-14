namespace Leanllm

inductive ClaudeModel where
  | opus55
  | sonnet55
  | haiku55
  | fable51
  | opus5
  | sonnet5
  | opus45
  | sonnet45
  | haiku45
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace ClaudeModel

def toModelId : ClaudeModel → String
  | .opus55     => "claude-opus-5-5"
  | .sonnet55   => "claude-sonnet-5-5"
  | .haiku55    => "claude-haiku-5-5"
  | .fable51    => "claude-fable-5-1"
  | .opus5      => "claude-opus-5"
  | .sonnet5    => "claude-sonnet-5"
  | .opus45     => "claude-opus-4-5-20251101"
  | .sonnet45   => "claude-sonnet-4-5-20250929"
  | .haiku45    => "claude-haiku-4-5-20251001"
  | .custom id  => id

def toOpenRouterSlug : ClaudeModel → String
  | .opus55     => "anthropic/claude-opus-5.5"
  | .sonnet55   => "anthropic/claude-sonnet-5.5"
  | .haiku55    => "anthropic/claude-haiku-5.5"
  | .fable51    => "anthropic/claude-fable-5.1"
  | .opus5      => "anthropic/claude-opus-5"
  | .sonnet5    => "anthropic/claude-sonnet-5"
  | .opus45     => "anthropic/claude-opus-4.5"
  | .sonnet45   => "anthropic/claude-sonnet-4.5"
  | .haiku45    => "anthropic/claude-haiku-4.5"
  | .custom id  => if id.contains '/' then id else s!"anthropic/{id}"

instance : ToString ClaudeModel where
  toString := toModelId

end ClaudeModel
end Leanllm
