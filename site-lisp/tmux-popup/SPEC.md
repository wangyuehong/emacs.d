# tmux-popup SPEC

本文件是 tmux-popup 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。

只描述使用者可观测的行为；实现手段在源码注释里，不在本文件。

## 术语

- popup frame：由带环境变量 `TMUX_POPUP=1` 的 emacsclient 打开的 frame。
  - tmux-open 在 tmux popup 中运行的 emacsclient 带这个环境变量。
- 按钮行：本包在窗口最上方加的一行，位于 buffer 自带的标题栏之上。
- 关闭按钮：按钮行右端的 `×`，与窗口右边缘隔一列空白。
- 右上角窗口：frame 中最靠上且最靠右的窗口。

## 全局约束

- 本包不修改 buffer 的标题栏：内容和宽度都与未开启本包时相同，有按钮行时显示在按钮行之下。
- 点击关闭按钮需要终端鼠标支持（`xterm-mouse-mode`），本包不负责开启。

## US-0010：popup frame 一直显示关闭按钮

作为用户，我希望 tmux popup 里的 Emacs 右上角一直有一个可点击的 `×`，在其中换文件、进 dired、分屏之后也还在。

### Background

- Given：`tmux-popup-mode` 已开启

### AC-0010-0010：右上角窗口显示关闭按钮

- When：popup frame 打开
- Then：右上角窗口多出按钮行，其中只有右端的 `×`

### AC-0010-0020：其他 frame 不显示

- When：打开不是 popup frame 的 frame
- Then：窗口与未开启本 mode 时相同，没有按钮行，不出现 `×`

- Examples:

  | frame |
  | --- |
  | 主 Emacs 的 frame，Emacs 进程自身带 `TMUX_POPUP=1` |
  | 不带 `TMUX_POPUP` 的 emacsclient 打开的 frame |
  | 带 `TMUX_POPUP=0` 的 emacsclient 打开的 frame |

### AC-0010-0030：换 buffer 后仍在

- Given：popup frame 显示着关闭按钮
- When：右上角窗口改为显示另一个 buffer
- Then：该窗口仍有按钮行，`×` 仍在其右端

- Examples:

  | 换 buffer 的方式 |
  | --- |
  | 打开另一个文件 |
  | 进入 dired |

### AC-0010-0040：窗口变化后跟到右上角窗口

- Given：popup frame 显示着关闭按钮
- When：frame 的窗口布局变化
- Then：frame 中只有一个 `×`，在变化后的右上角窗口

- Examples:

  | 布局变化 | 带 `×` 的窗口 |
  | --- | --- |
  | 上下分屏 | 上方的窗口 |
  | 左右分屏 | 右侧的窗口 |
  | 左右分屏后右侧再上下分屏，删去右上的窗口 | 右侧剩下的窗口 |

### AC-0010-0050：与 buffer 自带的标题栏共存

- Given：右上角窗口的 buffer 自带标题栏内容
- When：popup frame 显示该窗口
- Then：按钮行在上，标题栏在其下照常显示该内容
- Then：标题栏内容宽于窗口时，`×` 仍在按钮行右端

- Examples:

  | 标题栏内容 |
  | --- |
  | breadcrumb 显示的代码位置 |
  | 含全角字符的长文字 |
  | 用对齐空格排到窗口右端的列，如 `C-x C-b` 的表头 |

## US-0020：点击关闭按钮结束查看

作为用户，我希望点一下 `×` 就结束 popup 中的 Emacs，效果与在其中按 `C-x C-c` 相同。

### AC-0020-0010：点击后关闭

- Given：popup frame 显示着关闭按钮
- When：用鼠标左键点击 `×`
- Then：与在该 frame 中按 `C-x C-c` 相同：frame 关闭，打开它的 emacsclient 正常退出

### AC-0020-0020：有未保存的修改时先询问

- Given：点击打开的文件有未保存的修改
- When：点击 `×`
- Then：在该 frame 中询问是否保存，与 `C-x C-c` 相同

### AC-0020-0030：不结束 emacsclient 以外的 frame

- Given：`tmux-popup-close` 作用的 frame 不是 emacsclient 打开的，如把它另绑到主 Emacs 的鼠标按键上
- When：调用 `tmux-popup-close`
- Then：以 `user-error` 终止，Emacs 不退出，也不询问保存

## US-0030：开关关闭按钮

作为用户，我希望用一个 mode 开关关闭按钮，关掉后 popup 里不再有按钮行。

### AC-0030-0010：开启时已打开的 popup frame 随即显示

- Given：已有 popup frame 打开，`tmux-popup-mode` 未开启
- When：开启 `tmux-popup-mode`
- Then：这些 frame 的右上角窗口随即显示 `×`

### AC-0030-0020：关闭后恢复原样

- Given：popup frame 显示着关闭按钮
- When：关闭 `tmux-popup-mode`
- Then：所有 frame 的按钮行随即消失，不再出现 `×`
- Then：之后分屏、换 buffer 也不再出现 `×`

## 边界

- 用 tmux 的 popup 菜单把 popup 转成 pane 后，frame 仍是 popup frame，`×` 仍在；点击后 frame 关闭，pane 随之退出。
- popup frame 中用 `C-x 5 2` 等新开的 frame 不是 popup frame，不显示 `×`。
  - 点击原 frame 的 `×` 时，与 `C-x C-c` 一样，同一终端上的 frame 一起关闭。
- 浏览时另开并修改过的 buffer 不询问、不删除，修改留在 Emacs 中，与 `C-x C-c` 相同。
- 关闭由鼠标点击触发，`use-dialog-box` 非 nil 时询问以菜单显示，为 nil 时与 `C-x C-c` 一样在 minibuffer 询问。
- buffer 用 `tab-line-mode` 等自带 tab line 时，右上角窗口显示按钮行，不显示该 buffer 的 tab line。
