# md-tui-preview SPEC

本文件是 md-tui-preview 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。

## 术语

- 预览态：当前 buffer 处于 `md-tui-preview-mode`，只读，显示 Glow 渲染后的 ANSI 内容。
- 编辑态：当前 buffer 处于 `markdown-mode`（含 `gfm-mode` 等派生 mode），可编辑。

## 全局约束

- 失败语义沿用 `site-lisp/CLAUDE.md` 的 fail-fast 硬约束：`glow` 进程非零退出以 `user-error` 终止，不静默兜底。
- 渲染程序暂时硬编码为 `glow`，不支持替换为其他渲染器（如 `mdcat`）。
- 仅 `glow` 自身的命令行参数（`md-tui-preview-glow-args`）可配置。
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

> 说明：Glow 的 `dark` 内置样式会给 H1 标题、行内 `code` 等元素单独指定背景色，渲染出色块/横条。
> 这两个元素分别用 bright-blue、black 槽位画背景。
> 统一把所有槽位的背景改成 buffer 自身背景后，色块视觉上消失，只保留文字着色。
> 这样也不必依赖猜测「这个样式还会用哪些槽位画背景」。

### AC-0020-0030：渲染结束后还原 face

- Given：渲染调用结束（正常或异常路径）
- When：检查 16 个 `ansi-color-*` face 的前景色与背景色
- Then：与渲染前完全一致，不污染同 session 内其他 ansi-color 使用者（compile-mode、shell、magit 进程 buffer 等）

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

### AC-0025-0030：不保留 glow 自带的左侧留白

- Given：buffer 显示在某个窗口中
- When：进入预览
- Then：渲染内容左对齐到 buffer 最左列，不出现 glow「dark」风格自带的左侧留白列

> 说明：glow「dark」风格给每一行内容前都加了 2 列固定留白（glamour style 的 `document.margin`），
> 且 glow 没有对应的命令行 flag 可以关闭它（只有 `--style` 可选名称或完整 style JSON 路径）。
> 实现上是在 `ansi-color-apply-on-region` 跑完、转义序列已清空之后，对每一行检查开头是否恰好是这
> 2 个字面空格字符，若是则删除；空行或不以该留白开头的行不受影响。
> 这是当前实现选择的手段，不是本 AC 承诺的契约——只要渲染内容不再带有这段左侧留白，实现可以调整。

## US-0030：glow 参数可配置

作为用户，我希望能调整 Glow 的渲染参数（如换 style、加宽度限制），不改代码。

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
> 该观察针对 `glow --style dark` 验证：不使用 OSC 8（glow 未产生），也不向源文本注入唤醒标记。
>
> 按源码链接顺序，在渲染完成的纯文本中顺次正向搜索标签与目标文字（对内部空白容忍换行），命中区间打上内部文本属性。
>
> 搜索位置单调前进，重复标签/重复目标不会互相误配。
>
> 这是当前实现选择的手段，不是本 US 承诺的契约。
>
> 只要「光标在链接文字上按 RET 能跳转，不在链接上不跳转」这一结果成立即可。
