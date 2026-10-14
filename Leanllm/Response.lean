import Leanllm.Provider
import Leanllm.Models
import Leanllm.Message

namespace Leanllm

inductive StopReason where
  | endTurn
  | maxTokens
  | stopSequence (seq : Option String)
  | toolUse
  | contentFilter
  | other (raw : String)
  deriving Repr, BEq, Inhabited

structure Usage where
  inputTokens      : Nat
  outputTokens     : Nat
  cacheReadTokens  : Option Nat := none
  cacheWriteTokens : Option Nat := none
  reasoningTokens  : Option Nat := none
  deriving Repr, BEq, Inhabited

structure LLMResponse where
  id         : String
  model      : Model
  message    : Message
  stopReason : StopReason
  usage      : Option Usage := none
  deriving Repr, BEq, Inhabited

namespace LLMResponse

/-- Extract concatenated text from the assistant's response message. -/
def text (resp : LLMResponse) : String :=
  let texts := resp.message.content.filterMap fun
    | .text t => some t
    | _       => none
  String.intercalate "\n" texts

/-- Extract all `ToolCall`s emitted by the assistant in this response. -/
def toolCalls (resp : LLMResponse) : List ToolCall :=
  resp.message.content.filterMap fun
    | .toolCall tc => some tc
    | _            => none

/-- Extract any thinking/reasoning trace blocks from the response. -/
def thinking (resp : LLMResponse) : List String :=
  resp.message.content.filterMap fun
    | .thinking t _ => some t
    | _             => none

end LLMResponse
end Leanllm
