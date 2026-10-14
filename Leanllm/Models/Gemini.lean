namespace Leanllm

inductive GeminiModel where
  | argon4
  | flash3_8
  | flash3_5
  | pro2_5
  | flash2_5
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace GeminiModel

def toModelId : GeminiModel → String
  | .argon4    => "gemini-4-argon"
  | .flash3_8  => "gemini-3.8-flash"
  | .flash3_5  => "gemini-3.5-flash"
  | .pro2_5    => "gemini-2.5-pro"
  | .flash2_5  => "gemini-2.5-flash"
  | .custom id => id

def toOpenRouterSlug : GeminiModel → String
  | .custom id => if id.contains '/' then id else s!"google/{id}"
  | m          => s!"google/{m.toModelId}"

instance : ToString GeminiModel where
  toString := toModelId

end GeminiModel
end Leanllm
