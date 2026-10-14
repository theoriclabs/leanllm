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

end Leanllm
