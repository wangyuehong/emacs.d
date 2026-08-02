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
# 并删掉 code_block 的 chroma 与 color：代码块的着色改由 markdown-mode 的原生 fontify 施加
# （见 SPEC.md US-0070），glow 不再参与。
#   - chroma 整段删掉而非逐个改色：它的着色是固定 256 色、不跟随主题，
#     且对无法识别的语言会猜语言，猜错时把片段判为 Error 并画上背景色块。
#   - color（上游为 244）删掉：glow 会把它降成 SGR 90（bright black），
#     而主题取色映射把 black 槽位映射到 `default` 的背景色，代码文字会与背景同色而看不见。
#     删掉后代码文字走正文色。这两条保护的是仍会进入 glow 的块——缩进式（4 空格）代码块。
#   - margin（上游为 2）删掉：块内左缩进属 glow 的装饰性布局，与 document.margin 同类。
#
# 除此之外只改布局字段，不动任何颜色槽位——主题取色映射（见 md-tui-preview-core.el 的
# Theme Color Mapping 注释）依赖这些槽位保持不变。
.document.margin = 0
| .document.block_prefix = ""
| .document.block_suffix = ""
| .h1.prefix = "# "
| .h1.suffix = ""
| del(.code_block.chroma)
| del(.code_block.color)
| del(.code_block.margin)
