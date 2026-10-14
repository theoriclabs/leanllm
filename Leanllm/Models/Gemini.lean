namespace Leanllm

inductive GeminiModel where
  | argon4
  | flash38
  | flash35
  | pro25
  | flash25
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GeminiModel

def toModelId : GeminiModel → String
  | .argon4     => "gemini-4-argon"
  | .flash38    => "gemini-3.8-flash"
  | .flash35    => "gemini-3.5-flash"
  | .pro25      => "gemini-2.5-pro"
  | .flash25    => "gemini-2.5-flash"
  | .custom id  => id

def toOpenRouterSlug : GeminiModel → String
  | .custom id => if id.contains '/' then id else s!"google/{id}"
  | m          => s!"google/{m.toModelId}"

instance : ToString GeminiModel where
  toString := toModelId

end GeminiModel
end Leanllm
