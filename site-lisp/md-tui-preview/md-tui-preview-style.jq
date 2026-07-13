# 叠加在 glamour 上游 "dark" 风格之上、生成 md-tui-preview-style.json 的布局覆盖。
# 本文件是本包风格定制的 source of truth：改这里（而非改生成的 JSON），再 `make style`
# 重新生成；`make style-check` 校验生成的 JSON 仍等于「上游 + 本覆盖」，以防 drift。
#
# 当前覆盖把 glow "dark" 自带的装饰性布局字段在源头置零，从而无需任何渲染后的事后处理：
#   - document.margin        2  -> 0    （去掉每行左侧固定留白列）
#   - document.block_prefix "\n" -> ""  （去掉文档顶部空行）
#   - document.block_suffix "\n" -> ""  （去掉文档底部空行）
#   - h1.prefix             " " -> "# " （H1 用 `# ` 前缀，与 h2 的 `## `、h3 的 `### ` 一致）
#   - h1.suffix             " " -> ""   （去掉 H1 尾部用于画横条的空格填充）
#
# 只改布局字段，不动任何颜色槽位——主题取色映射（见 md-tui-preview-core.el 的
# Theme Color Mapping 注释）依赖这些槽位保持不变。
.document.margin = 0
| .document.block_prefix = ""
| .document.block_suffix = ""
| .h1.prefix = "# "
| .h1.suffix = ""
