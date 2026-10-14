import Lean.Data.Json
import Leanllm.Message

namespace Leanllm

inductive AuthErrorKind where
  | missingApiKey
  | invalidApiKey
  | insufficientPermissions
  deriving Repr, BEq, Inhabited

inductive RateLimitErrorKind where
  | requestsPerMinute (retryAfterMs : Option Nat)
  | tokensPerMinute   (retryAfterMs : Option Nat)
  | quotaExceeded
  deriving Repr, BEq, Inhabited

inductive InvalidRequestKind where
  | contextLengthExceeded (maxTokens : Option Nat)
  | unsupportedFeature    (feature : String)
  | invalidToolSchema     (toolName : String) (reason : String)
  | malformedParameters   (details : String)
  deriving Repr, BEq, Inhabited

inductive NetworkErrorKind where
  | timeout
  | dnsFailure
  | connectionRefused
  | tlsError
  | transport (details : String)
  deriving Repr, BEq, Inhabited

inductive ProviderErrorKind where
  | overloaded
  | internalError
  | badGateway
  | statusCode (code : Nat) (body : String)
  deriving Repr, BEq, Inhabited

inductive ParseErrorKind where
  | invalidJson      (raw : String) (err : String)
  | unexpectedSchema (expected : String) (actual : Lean.Json)
  | invalidToolArgs  (toolName : String) (raw : String)
  deriving Repr, BEq, Inhabited

inductive LLMError where
  | auth           (kind : AuthErrorKind)      (message : String)
  | rateLimit      (kind : RateLimitErrorKind) (message : String)
  | invalidRequest (kind : InvalidRequestKind) (message : String)
  | network        (kind : NetworkErrorKind)   (message : String)
  | provider       (kind : ProviderErrorKind)  (message : String)
  | parse          (kind : ParseErrorKind)     (message : String)
  | contentBlocked (reason : String)
  deriving Repr, BEq, Inhabited

end Leanllm
