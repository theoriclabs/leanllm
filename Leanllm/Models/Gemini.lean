namespace Leanllm

inductive GeminiModel where
  | pro25
  | flash25
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GeminiModel

def toModelId : GeminiModel → String
  | .pro25      => "gemini-2.5-pro"
  | .flash25    => "gemini-2.5-flash"
  | .custom id  => id

instance : ToString GeminiModel where
  toString := toModelId

end GeminiModel
end Leanllm
