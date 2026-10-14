import Lean.Data.Json

namespace Leanllm

instance : Repr Lean.Json where
  reprPrec j _ := j.compress

inductive ImageSource where
  | url    (url : String)
  | base64 (mediaType : String) (data : String)
  deriving Repr, BEq, Inhabited

structure ToolSpec where
  name        : String
  description : String
  parameters  : Lean.Json := Lean.Json.mkObj []
  deriving Repr, BEq, Inhabited

structure ToolCall where
  id        : String
  name      : String
  arguments : Lean.Json
  deriving Repr, BEq, Inhabited

structure ToolResult where
  toolCallId : String
  content    : String
  isError    : Bool := false
  deriving Repr, BEq, Inhabited

inductive ContentPart where
  | text       (text : String)
  | image      (source : ImageSource)
  | toolCall   (call : ToolCall)
  | toolResult (result : ToolResult)
  | thinking   (thought : String) (signature : Option String := none)
  deriving Repr, BEq, Inhabited

inductive Role where
  | system
  | developer
  | user
  | assistant
  | tool
  deriving Repr, BEq, Inhabited

structure Message where
  role    : Role
  content : List ContentPart
  name    : Option String := none
  deriving Repr, BEq, Inhabited

namespace Message

def system (text : String) : Message :=
  { role := .system, content := [.text text] }

def developer (text : String) : Message :=
  { role := .developer, content := [.text text] }

def user (text : String) : Message :=
  { role := .user, content := [.text text] }

def assistant (text : String) : Message :=
  { role := .assistant, content := [.text text] }

def toolResult (toolCallId : String) (content : String) (isError : Bool := false) : Message :=
  { role := .tool, content := [.toolResult { toolCallId, content, isError }] }

end Message
end Leanllm
