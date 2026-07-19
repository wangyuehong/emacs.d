# md-tui-preview SPEC

本文件是 md-tui-preview 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。

## 术语

- 预览态：当前 buffer 处于 `md-tui-preview-mode`，只读，显示 Glow 渲染后的 ANSI 内容。
- 编辑态：当前 buffer 处于 `markdown-mode`（含 `gfm-mode` 等派生 mode），可编辑。

## 全局约束

- 失败语义沿用 `site-lisp/CLAUDE.md` 的 fail-fast 硬约束：`glow` 进程非零退出以 `user-error` 终止，不静默兜底。`mermaid-ascii` 单块渲染失败改走 US-0080 AC-0080-0030 的显式状态分支，不中止整体渲染，但同样不静默——失败原因原样可见，判断依据是退出状态而非异常捕获，详见该 AC。
- 渲染程序暂时硬编码为 `glow`，不支持替换为其他渲染器（如 `mdcat`）。`mermaid-ascii`（见 US-0080）是喂给 glow 之前的前置预处理步骤，不是替代渲染器，不违反本条约束。
- 渲染风格是随包自带的 glow 风格文件（由 glamour 上游 `dark` 风格叠加布局覆盖生成），不用 glow 内置的 `dark`。
  - 自带的原因：主题取色映射（US-0020）依赖该风格输出的固定 ANSI 槽位，上游改槽位会破坏映射；自带即锁定槽位。
  - 布局覆盖由 `md-tui-preview-style.jq` 叠加在 pin 住的上游版本之上，生成 `md-tui-preview-style.json`。
  - `make style-check` 校验二者一致以防 drift，详见包内 Makefile。
- `md-tui-preview-glow-args` 可追加 glow 命令行参数；默认已含指向自带风格文件的 `--style`。
- 主题取色映射复用同一颜色给 normal/bright 两组 `ansi-color-*` face。
- 当前主题没有 16 个独立语义槽位，复用是最小化、诚实的映射，不是遗漏。
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

> 说明：`special-mode` 的只读只挡编辑命令，不挡 `save-buffer`——`buffer-read-only` 为 t 时仍能保存。
> 若不做处理，用户在预览态按下 `C-x C-s`（编辑习惯）会把渲染结果覆盖写入真实 `.md` 文件。
> 实现上是进入预览时清空 `buffer-file-name`（及 `buffer-auto-save-file-name`，见下）并保存原值，退出时恢复。
> 这是当前实现选择的手段，不是本 AC 承诺的契约——只要「不静默覆盖源文件」这一可观测结果成立，实现可以调整。

### AC-0010-0080：非 Markdown buffer 拒绝进入预览

- Given：当前 buffer 既不是 `markdown-mode` 也不是 `md-tui-preview-mode`
- When：调用 `md-tui-preview-toggle`
- Then：以 `user-error` 终止，不渲染、不切换 major mode

### AC-0010-0090：渲染失败后仍可用 toggle 恢复到编辑态

- Given：进入预览时 `glow` 渲染失败（以 `user-error` 终止）
- When：再次调用 `md-tui-preview-toggle`
- Then：切回进入预览前的原 major mode（不降级），文本与进入前逐字节一致

> 说明：渲染失败发生在 `md-tui-preview-mode` 的 body 已经跑完之后（body 只捕获状态，不
> 渲染）。因此原文本、原 major mode 等 `--source-*` 状态在失败前已经落定，恢复路径可以
> 照常工作，不会卡死在预览态。
> 这是当前实现选择的手段，不是本 AC 承诺的契约——只要「失败后仍能用 toggle 恢复、不降级」
> 这一可观测结果成立，实现可以调整。

## US-0020：配色跟随主题

作为用户，我希望预览的颜色和当前 Emacs 主题一致，而不是 Glow 自带的固定配色。

### AC-0020-0010：前景色取自主题

- Given：任意已加载的主题
- When：进入预览
- Then：红/绿/黄/蓝/洋红/青 6 个 `ansi-color-*` face 的前景色，取自当前 `ansi-color-names-vector` 对应槽位
- Then：bright 变体复用同一槽位的颜色
- Then：黑/白 2 个 face 的前景色改取自 `default` face 的背景/前景

> 说明：不从 `error`/`font-lock-keyword-face` 等语义 face 取色，因为这类 face 的色相未必和名字暗示的 ANSI 色一致。
> 例如 srcery 主题的 `font-lock-keyword-face` 是红色。
> 套进 Glow 样式里「蓝色」的槽位会产出色相错位的结果（如红底配黄字的 H1）。
> `ansi-color-names-vector` 是主题作者显式声明的、色相与槽位名一致的取色点。
> 即使 `ansi-color.el` 本身不再读取它做渲染，当前主题（srcery）仍手动设置了这个变量。
>
> 黑/白两槽单独改取 `default` face：不少主题把 ANSI「白」槽位留给一个偏暗/偏灰的色调（srcery 即是如此）。
> Glow 的正文段落大量使用这一槽位渲染普通文字，套用该色会让正文看起来像注释一样发灰。
> buffer 真正的正文色是 `default` 的前景色，不是主题对「ANSI 白」的定义。

### AC-0020-0020：背景色统一取自 buffer 自身背景，不出现色块

- Given：任意已加载的主题
- When：进入预览
- Then：全部 16 个 `ansi-color-*` face 的背景色，统一设为 `default` face 的背景色（与 buffer 本身背景一致）
- Then：不论 Glow 样式给哪个槽位指定了背景色，均不出现色块/横条

> 说明：自带风格（glamour `dark` 基底）会给 H1 标题、行内 `code` 等元素单独指定背景色，渲染出色块/横条。
> 这两个元素分别用 bright-blue、black 槽位画背景。
> 统一把所有槽位的背景改成 buffer 自身背景后，色块视觉上消失，只保留文字着色。
> 这样也不必依赖猜测「这个样式还会用哪些槽位画背景」。

### AC-0020-0030：渲染结束后还原 face

- Given：渲染调用结束（正常或异常路径）
- When：检查 16 个 `ansi-color-*` face 的前景色与背景色
- Then：与渲染前完全一致，不污染同 session 内其他 ansi-color 使用者（compile-mode、shell、magit 进程 buffer 等）

### 补充说明

- glow（截至 v2.1.2 / glamour v0.10.0）向捕获的非 tty 输出渲染时，不输出任何代码块背景色 ANSI 序列，即便风格文件里 `code_block` 声明了背景色也不生效。
  - 因此代码块的区分背景不经由 glow，而在 Emacs 侧对代码块区域叠加背景 overlay 实现，见 US-0070。

## US-0025：渲染宽度跟随窗口

作为用户，我希望预览的换行宽度和当前窗口的实际可用宽度一致，不是 Glow 在非 tty 下猜测的固定宽度。
也不希望因为忽略行号 gutter 占用的列数而溢出换行。

### AC-0025-0010：宽度取自当前窗口，且扣除行号 gutter

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：传给 glow 的 `--width` 等于 `window-body-width` 减去行号 gutter 实际占用的列数
- Then：仅当 `display-line-numbers-mode` 处于开启状态时才扣除

> 说明：`window-body-width` 的文档明确写了不包含 fringe / margin / 滚动条，但行号显示不属于这三者中任何一个。
> `window-body-width` 因此不会扣掉行号 gutter 的宽度。
> 直接把 `window-body-width` 传给 glow，会让每一行都比 gutter 出现后的真实可用宽度宽出 gutter 的列数。
> 结果是 Emacs 自己的换行逻辑对几乎每一行都做二次折行。
> 修法是显式减去 `(line-number-display-width 'columns)`，再加其文档说明的 2 列 padding。

### AC-0025-0020：渲染时机晚于行号 gutter 生效

- Given：`display-line-numbers-mode` 由父配置的 hook（`init-highlight.el`）而非本包自己开启
- When：进入预览
- Then：AC-0025-0010 的宽度计算结果，反映的是 `display-line-numbers-mode` 已经生效之后的窗口状态
- Then：不会出现「行号 gutter 还没出现时量出的宽度」这种时序错误

> 说明：实现上，渲染逻辑注册在 `md-tui-preview-mode-hook`、depth 90（默认 depth 是 0，数值更大越晚运行）。
> 这保证了其他 depth 0 的 hook 函数（包括 `display-line-numbers-mode`）已经跑完，`--effective-width` 才去测量窗口宽度。
> depth 90 是当前实现选择的手段，不是本 AC 承诺的契约——只要时序正确，实现可以调整。

### AC-0025-0030：宽度按文档行数预留 gutter，长文档不整体折行

- Given：源文档行数足以让行号位数多于 1 位（如上百行）
- When：进入预览
- Then：预览内容的每一行都不因行号 gutter 比测量时更宽而被 Emacs 二次折行（即不出现「每行下方多一条空续行」的整体双倍行距）

> 说明：glow 会把每一行用尾随空格填满到 `--width`，所以每一行的显示宽度都恰好等于传入的宽度。
> 若在渲染前于空 buffer 上测量宽度，行号 gutter 只按 1 行（1 位数）预留；插入上百行渲染结果后 gutter 变宽，
> 真实文本区随之变窄，那些满宽行就会超出文本区、逐行二次折行——比修复前的问题更明显。
> 修法是在 `erase-buffer` 之前、buffer 仍持有 Markdown 源文本时测量宽度，让 gutter 按文档自身行数（位数）预留，
> 与渲染后显示时的 gutter 位数一致。渲染后行数通常多于源文本（换行），但位数一般相同，故满宽行恰好贴合文本区。
> 这是当前实现选择的手段，不是本 AC 承诺的契约——只要长文档不整体折行，实现可以调整。

布局规整（去除 glow 自带的左侧留白等装饰性空白）见 US-0060。

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
- Then：以 `user-error` 终止，与「光标不在链接上」一致（图片不生成可跳转链接）

### 补充说明

- 边界情况：
  - 以下写法均不在本期支持范围内，不生成可跳转链接：
    - 标签内嵌套方括号（如 `[a[b]c](url)`）
    - shortcut 引用式 `[text]`（无显式 `[ref]`）
    - 标题片段 `file.md#heading`
    - 当前文档内 `#heading` 跳转
  - 未知协议（如 `javascript:`、`ftp:`）不生成可跳转链接。
  - 悬空引用（`[text][ref]` 无对应 `[ref]: target` 定义）不生成可跳转链接。
  - 若某条链接的标签或目标文字因渲染换行而未能在渲染后文本中准确定位，该条链接静默不可跳转，不影响其他链接。

> 说明：链接可跳转性建立在「渲染后文本里，链接标签与解析出的目标文字按源码顺序原样出现」这一观察之上。
>
> 该观察针对本包自带风格（glamour `dark` 叠加布局覆盖）验证：不使用 OSC 8（glow 未产生），也不向源文本注入唤醒标记。
>
> 按源码链接顺序，在渲染完成的纯文本中顺次正向搜索标签与目标文字（对内部空白容忍换行），命中区间打上内部文本属性。
>
> 搜索位置单调前进，重复标签/重复目标不会互相误配。
>
> 这是当前实现选择的手段，不是本 US 承诺的契约。
>
> 只要「光标在链接文字上按 RET 能跳转，不在链接上不跳转」这一结果成立即可。

## US-0060：预览内容左对齐、无 glow 装饰性空白

作为用户，我希望预览内容左对齐、紧贴 buffer，不带 glow 风格自带的装饰性留白，读起来和普通文本一样干净。

### AC-0060-0010：内容左对齐，无文档级左侧留白

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：正文、标题、列表等内容左对齐到 buffer 最左列，每行前不出现 glow 文档级的固定左侧留白列

> 说明：代码块正文保留 glow 的块内 2 列缩进，与 US-0070 的背景框叠加后表现为框内左侧 padding，
> 属刻意的代码框内留白，不在本 AC「文档级左留白」所指范围。

### AC-0060-0020：文档顶部、底部无多余空行

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：渲染内容的第一行即正文首行，最后一行即正文末行，首尾都不插入空行

### AC-0060-0030：H1 以 `# ` 前缀呈现，与其他各级标题一致

- Given：源文档含各级标题
- When：进入预览
- Then：H1 以 `# ` 前缀呈现，与 H2 的 `## `、H3 的 `### ` 等对应级数的 `#` 标记一致
- And：H1 左对齐，不出现用于画横条的前后空格填充或色条

### 补充说明

> 说明：这些布局特征都来自 glamour 上游「dark」风格的装饰性字段——`document.margin`（左留白）、
> `document.block_prefix`/`block_suffix`（顶部/底部空行）、`h1.prefix`/`suffix`（H1 横条的前后空格）。
> 本包不在渲染后事后剥离，而是在随包自带的风格文件里从源头规整（见「全局约束」）：
> 前三者直接置零；H1 的 `prefix` 由横条空格改为 `# `、`suffix` 置零，使 H1 与 H2–H6 一样以 `#` 标记开头。
> 这是当前实现选择的手段，不是本 US 承诺的契约——只要预览内容不带装饰性空白、且 H1 与其他标题呈现一致，实现可以调整。
> 上述字段均属布局，不改动任何颜色槽位，US-0020 的取色映射不受影响。

## US-0070：代码块有区分背景，便于辨识

作为用户，我希望预览里的代码块有一块与正文不同的背景色，一眼就能看出代码区的范围，而不是只靠语法高亮的文字颜色去分辨。

### Background

- Given：当前 buffer 处于 `md-tui-preview-mode`
- Given：渲染内容含至少一个围栏代码块（`` ``` `` 或 `~~~`）

### AC-0070-0010：围栏代码块整体获得区分背景

- Given：渲染内容含一个围栏代码块
- When：进入预览
- Then：该代码块从首行到末行、每行铺满整行宽度（延伸到窗口右缘，不止文字部分）显示与正文不同的背景色，含块内空行与因过长而折行的续行

### AC-0070-0020：背景不改变代码的语法高亮前景色

- Given：代码块内容被 glow 施加了语法高亮（chroma 前景色）
- When：进入预览
- Then：叠加区分背景后，代码文字的前景色保持不变

### AC-0070-0030：背景色默认跟随主题，可自定义或关闭

- When：进入预览
- Then：`md-tui-preview-code-block-background` 决定背景色来源

- Examples:
  | `md-tui-preview-code-block-background` | 代码块背景 |
  | --- | --- |
  | `auto`（默认） | 取自 buffer 背景的相近色调，暗色主题调亮、亮色主题调暗，两种主题下都与正文可区分 |
  | 某颜色字符串 | 使用该颜色 |
  | `nil` | 不叠加背景（关闭本功能） |

### 补充说明

> 说明：glow 不输出代码块背景（见 US-0020 补充说明），故背景在 Emacs 侧实现：
> 解析源文本里的围栏代码块，在渲染结果中定位其区域，叠加一个只设背景色的匿名 overlay face。
> overlay 背景压过被 US-0020 归一到 buffer 背景的文字背景，而语法高亮的前景色透过 overlay 保留，因此只改背景、不改前景。
> glow 会把每行填充到传入的渲染宽度（即 `md-tui-preview--effective-width` 算出的可用文本宽度），
> 故 overlay 覆盖的尾随空格实体单元本身已把背景铺到窗口右缘，无需 `:extend`。
>
> 定位沿用链接的做法（见 US-0050）：`goto-char` 到 `point-min` 一次，把代码块的每一非空内容行按源码顺序逐行单调正向匹配，
> overlay 从首个匹配行覆盖到末个匹配行的下一行行首。逐行推进使块内重复文字各自命中自己的渲染位置
> （否则末行文字若与块内某行相同，两点式首尾搜索会止步于较早的那一处，只着色到块的上半）；
> 任一内容行无法定位时整块静默不加背景，不部分着色，不影响其他块。
>
> 这是当前实现选择的手段，不是本 US 承诺的契约——只要代码块显示区分背景、且不改文字前景色，实现可以调整。
>
> 边界：
>
> - 仅支持围栏代码块（`` ``` `` / `~~~`）；缩进式（4 空格）代码块不在本期范围。
> - chroma 语法高亮的前景色是固定 256 色、不跟随主题（既有属性，非本功能引入）；亮色主题下这些前景色与自动派生的亮背景可能对比偏低。

## US-0080：Mermaid 图表渲染为可视图形

作为用户，我希望预览里的 Mermaid 图表渲染成图形，而不是原始的图表描述文本，这样不必切换到浏览器或图形工具就能看懂图的结构。

### Background

- Given：当前 buffer 处于 `md-tui-preview-mode`
- Given：渲染内容含至少一个围栏代码块，其 info-string 首个空白分隔 token（大小写不敏感）为 `mermaid`

### AC-0080-0010：渲染工具可用时显示为图形

- Given：PATH 中能找到 `mermaid-ascii`
- When：进入预览
- Then：该块显示为图形化的图案，而非未渲染的图表描述文本

### AC-0080-0020：渲染工具不可用时原样呈现

- Given：PATH 中找不到 `mermaid-ascii`
- When：进入预览
- Then：该块按普通围栏代码块原样渲染，与未引入本功能前的行为一致
- Then：不报错、不提示消息

> 说明：这是显式的合法状态分支（工具是否存在），不是异常捕获后的静默兜底——判断发生在调用前，不涉及任何失败路径。

### AC-0080-0030：单块渲染失败被局部隔离，不影响其余内容

- Given：文档中某个 mermaid 块，其内容会导致渲染工具以非零状态退出
- When：进入预览
- Then：该块显示为原始的图表描述文本，并附带一行可见的失败原因（渲染工具的原始错误输出）
- Then：文档其余内容（包括其他能成功渲染的 mermaid 块）不受影响，照常渲染

> 说明：判断依据是渲染工具的退出状态——一个显式条件分支，不是靠捕获 `--call-mermaid-ascii`（不抛错的底层调用）抛出的异常来兜底；这条分支本身即是 SPEC 化的合法状态分支，满足「全局约束」fail-fast 条款「由显式条件区分，而非异常捕获」的要求。失败原因必须原样可见，不能被压成默认值或空白——这就是「不静默」的可观测面。

### AC-0080-0040：渲染参数可配置

- Given：`md-tui-preview-mermaid-ascii-args` 被设为非默认值
- When：下一次渲染某 mermaid 块
- Then：实际传给渲染工具的参数使用新值

### 补充说明

> 说明：替换发生在喂给 glow 之前的一份渲染专用文本上，不是 `md-tui-preview--source`（toggle 退出时逐字节还原用的那份）。glow 渲染、US-0050 的链接定位、US-0070 的背景定位三者统一改读这份替换后的文本——背景 / 链接定位靠在渲染后文本里搜索源码内容，若仍喂它们替换前的原始 mermaid 源码，会在图形化后的渲染结果里找不到对应内容行。
>
> 图形本身仍包裹在一个围栏代码块内：一是让 glow 把它当预格式化文本原样保留，不按段落重排/折行（图形对齐依赖每行逐字保留）；二是使其如常获得 US-0070 的区分背景，无需额外代码。
>
> 该围栏显式标记语言为 `text`，不是不带任何语言标记：glow 的语法高亮（chroma）对不带语言标记的围栏块，内容行数足够多时会启动语言猜测，猜错后会把猜中语言解析不出来的片段（常见诱因是 CJK 字符、全角标点混入）染上刺眼的背景色，误报为语法错误——已用真实 `glow`/`mermaid-ascii` 二进制复现，无论是 mermaid-ascii 的图形输出还是本功能的失败兜底原始源码，只要行数够多都会触发。显式打上 `text` 标记让 chroma 直接跳过猜测，按纯文本处理，达到「不带语言标记」原本想要的效果。
>
> 这是当前实现选择的手段，不是本 US 承诺的契约——只要「工具可用则显示为图形、不可用则原样渲染、单块渲染失败不影响其余内容且失败原因可见」这些可观测结果成立，实现可以调整。
>
> 边界：
>
> - 渲染工具没有约束图形宽度的参数，图形宽度只取决于图表内容与工具自身的间距设置；图形过宽时会被 glow 按渲染宽度折行，破坏图形对齐——已知、未解决，与任何过宽围栏代码块目前的处境同类。
> - 本功能可能让渲染后总行数相对原始源文本大幅增长；US-0025 的行号 gutter 位数是按替换前的源文本预留的，若这次增长恰好跨过数位边界（如 9 行以内的文档展开出两位数行数的图形），可能重现 AC-0025-0030 描述的整体二次折行——已知、未解决的新压力。
> - 一个 mermaid 标记的围栏块若在文档末尾前未闭合，不参与本功能的替换，按普通围栏代码块交给 glow 原样渲染——比 US-0070 的解析更保守：背景着色作用于不完整块无害，但把不完整内容喂给外部渲染工具再嵌回原文语义不清。
> - 渲染工具自身若无法识别图表内部某一行的具体写法，可能不会因此以非零状态退出，而是按其自身规则退化处理并仍渲染出图形——这是渲染工具自身的行为，不是本包引入的静默兜底，不在 AC-0080-0030 的失败语义覆盖范围内。
> - 渲染工具对 CJK（中日韩）字符的宽度计算不完整：`flowchart` 图的节点框、边标签能正确按显示宽度（CJK 双宽）留白对齐；但 `sequenceDiagram` 图的消息标签若含 CJK 字符，留白按字符数而非显示宽度计算，标签越长错位越明显，破坏该行与相邻行的竖线对齐——这是渲染工具自身的实现缺陷，不是本包集成方式引入的问题，也不在本包侧可修复范围内。
