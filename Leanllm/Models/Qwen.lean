namespace Leanllm

inductive QwenModel where
  | qwen38Max
  | qwen38Flash
  | qwen37Max
  | qwen37Plus
  | qwen35Plus
  | coderNext
  | coderPlus
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace QwenModel

def toModelId : QwenModel → String
  | .qwen38Max   => "qwen3.8-max-0902"
  | .qwen38Flash => "qwen3.8-flash"
  | .qwen37Max   => "qwen3.7-max"
  | .qwen37Plus  => "qwen3.7-plus"
  | .qwen35Plus  => "qwen3.5-plus"
  | .coderNext   => "qwen3-coder-next"
  | .coderPlus   => "qwen3-coder-plus"
  | .custom id   => id

def toOpenRouterSlug : QwenModel → String
  | .coderPlus => "qwen/qwen3-coder-next"
  | .custom id => if id.contains '/' then id else s!"qwen/{id}"
  | m          => s!"qwen/{m.toModelId}"

instance : ToString QwenModel where
  toString := toModelId

end QwenModel
end Leanllm
