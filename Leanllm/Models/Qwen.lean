namespace Leanllm

inductive QwenModel where
  | qwen3_8Max
  | qwen3_8Flash
  | qwen3_7Max
  | qwen3_7Plus
  | qwen3_5Plus
  | coderNext
  | coderPlus
  | custom (id : String)
  deriving Repr, BEq, Inhabited

namespace QwenModel

def toModelId : QwenModel → String
  | .qwen3_8Max   => "qwen3.8-max-0902"
  | .qwen3_8Flash => "qwen3.8-flash"
  | .qwen3_7Max   => "qwen3.7-max"
  | .qwen3_7Plus  => "qwen3.7-plus"
  | .qwen3_5Plus  => "qwen3.5-plus"
  | .coderNext    => "qwen3-coder-next"
  | .coderPlus    => "qwen3-coder-plus"
  | .custom id    => id

def toOpenRouterSlug : QwenModel → String
  | .coderPlus => "qwen/qwen3-coder-next"
  | .custom id => if id.contains '/' then id else s!"qwen/{id}"
  | m          => s!"qwen/{m.toModelId}"

instance : ToString QwenModel where
  toString := toModelId

end QwenModel
end Leanllm
