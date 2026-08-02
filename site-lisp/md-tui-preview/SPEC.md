# md-tui-preview SPEC

本文件是 md-tui-preview 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。
只描述使用者可观测的行为；实现手段与「为何这样实现」在源码注释里，不在本文件。

## 术语

- 预览态：当前 buffer 处于 `md-tui-preview-mode`，只读，显示 Glow 渲染后的内容。
- 编辑态：当前 buffer 处于 `markdown-mode`（含 `gfm-mode` 等派生 mode），可编辑。

## 全局约束

- 失败语义沿用 `site-lisp/CLAUDE.md` 的 fail-fast 硬约束：`glow` 进程非零退出以 `user-error` 终止，不静默兜底。
- 渲染程序硬编码为 `glow`，不支持替换为其他渲染器（如 `mdcat`）。
- 渲染风格是随包自带的风格文件，不用 glow 内置的 `dark`；其内容与生成方式以 `md-tui-preview-style.jq`
  为 SoT，`make style-check` 校验产物与之一致。
- `md-tui-preview-glow-args` 可追加 glow 命令行参数；默认已含指向自带风格文件的 `--style`。
- 只有代码块与编辑态同源（US-0070）。其余元素由 glow 按自带风格渲染，配色经 US-0020 的主题槽位映射，
  因此标题、行内代码、链接等的颜色与编辑态不对应，这是刻意的分工，不是缺陷。
- 主题取色映射对 normal 与 bright 两组 `ansi-color-*` face 复用同一颜色：当前主题没有 16 个独立语义槽位，
  复用是最小化、诚实的映射。
- 切换命令全程只操作当前 buffer、当前窗口，不 `pop-to-buffer`/`switch-to-buffer`。
- 本包不管理 evil 状态。进入/退出预览后 evil 状态由 evil 自身按 buffer 惯常规则决定。

## US-0010：预览与编辑互相切换

作为用户，我希望在编辑 Markdown 与查看其终端渲染效果之间快速切换，而不改变默认的编辑体验。

### AC-0010-0010：默认打开不受影响

- Given：安装了 `glow` 且在 TUI Emacs 中
- When：打开一个 `.md` 文件
- Then：buffer 仍是普通 `markdown-mode`，不自动进入预览

### AC-0010-0020：进入预览

- Given：当前 buffer 处于 `markdown-mode`
- When：调用 `md-tui-preview-toggle`
- Then：当前 buffer（同一个 buffer、同一个窗口）切换为 `md-tui-preview-mode`，只读
- Then：内容是 Glow 渲染后的当前 buffer 文本（含未保存的编辑）

### AC-0010-0030：退出预览

- Given：当前 buffer 处于 `md-tui-preview-mode`
- When：再次调用 `md-tui-preview-toggle`
- Then：当前 buffer 切回进入预览前的原 major mode（如 `gfm-mode`），不降级为通用 `markdown-mode`
- Then：文本与进入预览前逐字节一致
- Then：光标回到进入前的位置（clamp 到恢复后文本的长度内）

### AC-0010-0040：往返不丢失未保存的编辑

- Given：`markdown-mode` buffer 有未保存的编辑
- When：进入预览再退出
- Then：编辑内容未丢失，`buffer-modified-p` 仍为 t

### AC-0010-0050：往返不误改已保存 buffer 的 modified 标记

- Given：`markdown-mode` buffer 未被修改（`buffer-modified-p` 为 nil）
- When：进入预览再退出
- Then：`buffer-modified-p` 全程保持 nil

### AC-0010-0060：快捷键

- `markdown-mode` 中 `C-c C-c g`（`markdown-mode-command-map` 新增的 `g` 条目）触发 `md-tui-preview-toggle`
- 预览态中 `C-c C-c` 触发 `md-tui-preview-toggle`
- 预览态中 `q` 触发 `md-tui-preview-toggle`

### AC-0010-0070：预览态禁止 `save-buffer` 污染源文件

- Given：当前 buffer 处于 `md-tui-preview-mode`
- When：调用 `save-buffer`（如 `C-x C-s`）
- Then：不写入源文件；`save-buffer` 转为交互式「另存为」提示，不会静默覆盖
- Then：退出预览后，`save-buffer`/`C-x C-s` 恢复为直接保存回原文件路径

### AC-0010-0080：只能从 Markdown buffer 进入预览

- Given：当前 buffer 既不是 `markdown-mode` 也不是 `md-tui-preview-mode`
- When：调用 `md-tui-preview-toggle`
- Then：以 `user-error` 终止，不渲染、不切换 major mode
- Then：`md-tui-preview-mode` 本身不是命令，无法用 `M-x` 绕过上面这条检查

### AC-0010-0090：渲染失败后仍可用 toggle 恢复到编辑态

- Given：进入预览时 `glow` 渲染失败（以 `user-error` 终止）
- When：再次调用 `md-tui-preview-toggle`
- Then：切回进入预览前的原 major mode（不降级），文本与进入前逐字节一致

## US-0020：配色跟随主题

作为用户，我希望预览的颜色和当前 Emacs 主题一致，而不是 Glow 自带的固定配色。

### AC-0020-0010：前景色取自主题

- Given：任意已加载的主题
- When：进入预览
- Then：红/绿/黄/蓝/洋红/青 6 个 `ansi-color-*` face 的前景色，取自当前 `ansi-color-names-vector` 对应槽位
- Then：bright 变体复用同一槽位的颜色
- Then：黑/白 2 个 face 的前景色改取自 `default` face 的背景/前景

### AC-0020-0020：背景色统一取自 buffer 自身背景，不出现色块

- Given：任意已加载的主题
- When：进入预览
- Then：全部 16 个 `ansi-color-*` face 的背景色，统一设为 `default` face 的背景色
- Then：不论 Glow 样式给哪个槽位指定了背景色，均不出现色块/横条

### AC-0020-0030：渲染结束后还原 face

- Given：渲染调用结束（正常或异常路径）
- When：检查 16 个 `ansi-color-*` face 的前景色与背景色
- Then：与渲染前完全一致，不污染同 session 内其他 ansi-color 使用者（compile-mode、shell、magit 进程 buffer 等）

## US-0025：渲染宽度跟随窗口

作为用户，我希望预览的换行宽度和当前窗口的实际可用宽度一致，不是 Glow 在非 tty 下猜测的固定宽度，
也不希望因为忽略行号 gutter 占用的列数而溢出换行。

### AC-0025-0010：宽度取自当前窗口，且扣除行号 gutter

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：传给 glow 的 `--width` 等于 `window-body-width` 减去行号 gutter 实际占用的列数，再减 2 列右边余量
- Then：gutter 仅当 `display-line-numbers-mode` 处于开启状态时才扣除，右边余量始终扣除
- Then：预览内容的任何一行都不因触到窗口右缘而被 Emacs 软折行

### AC-0025-0020：渲染时机晚于行号 gutter 生效

- Given：`display-line-numbers-mode` 由父配置的 hook（`init-highlight.el`）而非本包自己开启
- When：进入预览
- Then：AC-0025-0010 的宽度计算结果，反映的是 `display-line-numbers-mode` 已经生效之后的窗口状态

### AC-0025-0030：宽度按文档行数预留 gutter，长文档不整体折行

- Given：源文档行数足以让行号位数多于 1 位（如上百行）
- When：进入预览
- Then：预览内容的每一行都不因行号 gutter 比测量时更宽而被 Emacs 二次折行（即不出现整体双倍行距）

> 边界：源文档行数恰在 9 / 99 / 999 以下、而渲染后跨过该边界时，gutter 会比测量时宽一列；
> AC-0025-0010 的右边余量吸收这一列，再多则会整体二次折行。

## US-0030：glow 参数可配置

作为用户，我希望能追加 Glow 的渲染参数（如临时加一个 glow 标志），不改代码。

### AC-0030-0010：自定义参数生效

- Given：`md-tui-preview-glow-args` 被设为非默认值
- When：下一次渲染
- Then：实际传给 `glow` 的参数使用新值（追加固定的 stdin 标记 `-`）

## US-0040：仅 TUI + 装了 glow 时才生效

作为用户，我不希望这个功能在 GUI Emacs 或没装 `glow` 的机器上引入任何行为差异或报错。

### AC-0040-0010：GUI Emacs 不加载

- Given：图形界面 Emacs（`display-graphic-p` 为真）
- When：启动
- Then：本包不加载，`.md` 文件行为与未安装该包时完全一致

### AC-0040-0020：未装 glow 不加载

- Given：`PATH` 中找不到 `glow`
- When：启动
- Then：本包不加载，`.md` 文件行为与未安装该包时完全一致，无报错

## US-0050：预览态中通过链接跳转到目标

作为用户，我希望在预览态阅读 Markdown 时，光标移到链接文字上按 RET 就能直接打开链接目标。
外部网址用浏览器打开，本地文件用 Emacs 打开，不必退出预览手动查找。

### Background

- Given：当前 buffer 处于 `md-tui-preview-mode`
- Given：渲染内容中包含至少一条可解析的链接

### AC-0050-0010：光标在外部链接文字上按 RET 打开浏览器

- Given：光标位于某条外部链接（http/https/mailto）渲染出的文字范围内
- When：按 RET
- Then：以该链接的目标地址调用 `browse-url` 打开

- Examples:
  | 源 Markdown 写法 | 目标地址 |
  | --- | --- |
  | `[文字](https://example.com)` | `https://example.com` |
  | `<https://example.com>` | `https://example.com` |
  | `[文字](mailto:a@b.com)` | `mailto:a@b.com` |
  | `[文字][ref]` + `[ref]: https://example.com` | `https://example.com` |

### AC-0050-0020：光标在本地文件链接文字上按 RET 打开该文件

- Given：光标位于某条本地文件链接（相对路径或绝对路径）渲染出的文字范围内，且目标文件存在
- When：按 RET
- Then：用 `find-file` 打开该目标文件，相对路径以当前源 Markdown 文件所在目录解析

- Examples:
  | 源 Markdown 写法 | 解析后目标 |
  | --- | --- |
  | `[文字](relative/file.md)` | 源文件所在目录 + `relative/file.md` |
  | `[文字](/absolute/file.md)` | `/absolute/file.md` |
  | `[文字][ref]` + `[ref]: relative/file.md` | 源文件所在目录 + `relative/file.md` |

### AC-0050-0030：光标不在链接上按 RET 不触发导航

- Given：光标位于预览态中不属于任何已解析链接的位置
- When：按 RET
- Then：以 `user-error` 终止，不调用 `browse-url` 或 `find-file`

### AC-0050-0040：本地目标不存在时按 RET 报错而不新建文件

- Given：光标位于某条本地文件链接渲染出的文字范围内，且解析后的目标路径不存在
- When：按 RET
- Then：以 `user-error` 终止，不调用 `find-file`（避免其新建空文件）

### AC-0050-0050：图片链接不可导航

- Given：渲染内容中包含图片语法（行内 `![alt](target)` 或引用式 `![alt][ref]`）渲染出的目标文字
- When：光标位于该目标文字范围内按 RET
- Then：以 `user-error` 终止，与「光标不在链接上」一致

### 边界

- 以下写法不生成可跳转链接：标签内嵌套方括号（`[a[b]c](url)`）、shortcut 引用式 `[text]`、
  标题片段 `file.md#heading`、文档内 `#heading` 跳转、未知协议（`javascript:`、`ftp:`）、
  悬空引用（`[text][ref]` 无对应定义）。
- 若某条链接的标签或目标文字未能在渲染后文本中定位，该条链接静默不可跳转，不影响其他链接。
- 预览里显示的本地路径是 glow 自己归一化的结果，相对路径会显示成看似根绝对的形式，与源码写法不同；
  契约只覆盖 RET 的跳转目标（按源文件所在目录解析），不覆盖显示文字。因此该显示文字通常也不可跳转，
  可跳转的是链接标签。

## US-0060：预览内容左对齐、无 glow 装饰性空白

作为用户，我希望预览内容左对齐、紧贴 buffer，不带 glow 风格自带的装饰性留白，读起来和普通文本一样干净。

### AC-0060-0010：内容左对齐，无文档级左侧留白

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：正文、标题、列表等内容左对齐到 buffer 最左列，每行前不出现 glow 文档级的固定左侧留白列

### AC-0060-0020：文档顶部、底部无多余空行

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：渲染内容的第一行即正文首行，最后一行即正文末行，首尾都不插入空行
- Then：文档以列表或引用块开头时同样如此

### AC-0060-0030：H1 以 `# ` 前缀呈现，与其他各级标题一致

- Given：源文档含各级标题
- When：进入预览
- Then：H1 以 `# ` 前缀呈现，与 H2 的 `## `、H3 的 `### ` 等对应级数的 `#` 标记一致
- And：H1 左对齐，不出现用于画横条的前后空格填充或色条

### 边界

- 表格由 glow 按渲染宽度铺满，列宽会远宽于内容所需，比编辑态的源码更稀疏。已知、不处理：
  表格与其余非代码块元素一样交给 glow（见「全局约束」）。

## US-0070：代码块的呈现与编辑态一致

作为用户，我希望预览里的代码块和我在编辑态看到的一样：从开头的 ```` ```语言 ```` 到结尾的 ```` ``` ````
逐字不变，着色也相同、同样跟随当前主题。不要另一套外部渲染器的排版与配色，也不要本包自己另画一套。

### Background

- Given：当前 buffer 处于 `md-tui-preview-mode`
- Given：源文档含至少一个围栏代码块（`` ``` `` 或 `~~~`），可带任意缩进

### AC-0070-0010：呈现与编辑态同源

- Given：某围栏代码块的语言在编辑态能被 markdown-mode 原生着色（如 `mermaid` 对应 `mermaid-mode`）
- When：进入预览
- Then：该块显示为源文档原文：开头的围栏行（含语言标注）、内容各行、结尾的围栏行，逐字一致，
  无额外缩进、无按渲染宽度的折行与填充
- Then：围栏标记、语言标注、块内容的着色与编辑态一致，颜色取自当前主题
- Then：块整体叠加 `markdown-code-face`，外观与编辑态一致

### AC-0070-0020：无法原生着色的块仍按源码原样呈现

- Given：某围栏代码块的语言在编辑态也得不到原生着色（无对应 mode、或原生着色被关闭）
- When：进入预览
- Then：该块仍按源码原样显示（围栏、语言标注、内容都在），呈现与编辑态对同一块的呈现一致
- Then：不报错、不提示消息，文字可正常阅读

### AC-0070-0030：代码块不出现外部渲染器的配色与排版

- When：进入预览
- Then：代码块区域内不出现 glow 的固定 256 色前景，不出现 glow 画的背景色块，也不出现 glow 的块内缩进
- Then：未被着色的代码文字不与 buffer 背景同色

### AC-0070-0040：单块失败被局部隔离

- Given：某个块无法被放回渲染结果中的对应位置
- When：进入预览
- Then：该块不出现在预览里，其它块与其余内容照常呈现，不报错

### 边界

- 缩进式（4 空格）代码块不在本期范围：它们仍由 glow 渲染，没有围栏行也没有 markdown-mode 的着色。
- 代码块不受渲染宽度约束：源码里的长行在预览里也是长行，由 Emacs 按窗口软折行，与编辑态一致。
- 放回的块行数与占位不同，会改变渲染后总行数；若恰好跨过行号位数边界，可能重现 AC-0025-0030 的边界。
- 块内的链接不可跳转：渲染结果里那处只有占位标记，链接定位看不到块内容。
- markdown-mode 的着色若依赖 buffer 之外的上下文（如 LSP、项目配置），在预览里得不到，可能弱于编辑态。
