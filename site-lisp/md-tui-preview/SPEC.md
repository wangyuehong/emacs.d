# md-tui-preview SPEC

本包把终端浏览器嵌入 Emacs TUI，并用 mdv 预览已保存的 Markdown 文件。

## 术语与边界

- 浏览器层：`terminal-browser-embed.el` 接收 URL，管理当前窗口、图形协议、输入和资源清理，不解析 Markdown。
- Markdown 层：`md-tui-preview.el` 为已保存的 `.md` 文件启动本地 mdv 服务，生成样式，再把服务 URL 交给浏览器层。
- 支持环境：Emacs 31+ TUI、支持 Kitty 图形协议的终端，以及 `terminal-browser`、`mdv`。
  - Emacs 可直接运行在该终端中，也可运行在开启 `allow-passthrough` 的 tmux 中。
- mdv v0.1.9 提供 Markdown 到 HTML、代码高亮和官方 Mermaid 渲染；浏览器资源由 mdv 内置，离线可用。

## US-0010：从编辑进入预览

作为 Markdown 编辑者，我希望打开已保存文件的预览，退出后回到原处。

### AC-0010-0010：已保存文件与编辑状态

- Given：当前 buffer 使用 `markdown-mode` 或派生 mode，并关联已保存、可读的 `.md` 文件。
- When：调用 `md-tui-preview-toggle`。
- Then：当前窗口显示独立只读预览；源 buffer 的文本、undo、mode、point、文件名和 modified 标记不变。
- And：未关联文件、未保存修改、非 `.md` 文件或非 Markdown mode 均报 `user-error`，不启动预览。

### AC-0010-0020：退出与重复打开

- Given：当前终端已有预览。
- When：按 `q`、`C-c C-c`，或从另一个已保存 `.md` buffer 再次进入预览。
- Then：旧预览关闭，只停止其浏览器与 mdv 子进程并清理临时文件；退出时恢复起始 buffer 和位置。
- And：窗口已显示别的 buffer 时不覆盖它；其他终端的预览不受影响。

## US-0020：阅读 Markdown

作为阅读者，我希望按当前配色和字体查看正文、代码与图表。

### AC-0020-0010：mdv 渲染

- Given：已保存的 `.md` 文件包含正文、表格、代码围栏、本地图片和 Mermaid 围栏。
- When：进入预览。
- Then：mdv 从磁盘读取文件，输出 HTML、代码高亮与 Mermaid；本地资源相对于该文件目录解析。
- And：不启动系统浏览器，不从网络加载渲染依赖，目录栏和主题切换按钮不显示。

### AC-0020-0020：样式与刷新

- Given：Emacs 当前主题和预览字体配置有效。
- When：进入预览，或修改主题后在预览中按 `g r`。
- Then：正文与标题使用相应 Emacs face 的颜色；正文、代码与 Mermaid 使用配置的等宽字体，标题按层级放大。
- And：字体默认值是 `Maple Mono Normal NL NF CN` 和 12pt。
- And：用 `my/md-preview-font-family`、`my/md-preview-font-size` 调整字体。
- And：刷新重写样式并重新加载浏览器页面；保存文件后 mdv 自动更新正文。

## US-0030：在 Emacs 窗口阅读

作为终端使用者，我希望预览保留浏览器的滚动、点击和历史操作。

### AC-0030-0010：URL 嵌入边界

- Given：调用 `my/terminal-browser-open-url` 并传入 URL。
- When：当前 Emacs 是支持图形协议的 TUI，直接运行在终端中或运行在 tmux 中。
- Then：浏览器层在当前窗口启动无地址栏的 terminal-browser，显示该 URL，不读取 Markdown buffer，也不启动 mdv。
- And：不支持的终端或浏览器启动失败时报告原因并释放本次资源。

### AC-0030-0020：导航与图像

- Given：预览含长文和链接。
- When：使用方向键、`j/k`、`C-b/f`、`C-u/d`、`g g`、`G`、`h/l`、`Tab/RET`、鼠标或滚轮。
- Then：浏览器滚动、翻页、后退、前进、聚焦或激活链接；`q` 与 `C-c C-c` 返回编辑。
- And：窗口尺寸变化后浏览器适配；图形中继不把浏览器标题或其他终端控制序列写入 Emacs 画面。

## US-0040：链接与服务范围

作为 Markdown 阅读者，我希望知道 mdv 服务实际支持哪些链接。

### AC-0040-0010：链接行为

- Given：当前 mdv 服务以起始 `.md` 文件所在目录为根。
- When：激活同目录树内另一 `.md` 文件的相对链接。
- Then：浏览器在同一页签导航，可用 `h/l` 后退和前进；`q` 返回最初的 Emacs buffer。
- And：`.markdown` 目标及不在 mdv 静态文件白名单内的目标由 mdv 拒绝。
- And：外部链接由浏览器自行打开，不调用 Emacs 的 `browse-url`。
- And：mdv 只监听回环地址，不通过 Emacs 动作桥处理链接。

### AC-0040-0020：mdv 启动与清理

- Given：目标 `.md` 文件可读且 mdv 可执行。
- When：启动或退出预览。
- Then：Markdown 层以 `--no-open` 启动 mdv，从 stderr 的启动信息取得实际端口并构造 URL；退出时只停止本次 mdv 子进程。
- And：启动失败或服务意外退出时报告原因，释放浏览器与临时资源，不留下预览窗口。
