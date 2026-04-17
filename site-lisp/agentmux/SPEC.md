# agentmux SPEC

本文件是 agentmux 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。

## 术语

- Agent pane：tmux 中运行 AI CLI（Claude Code / Gemini CLI 等）的目标 pane
- 目标：由 `emamux:target-session` 解析出的 session/window/pane 三元组
- 提交：Agent 接收到一条完整消息并开始处理
- 暂存：内容进入 Agent 输入框但未提交，用户可继续编辑
- 上下文：文件路径、行号或区域范围，以及可选的代码内容片段

## 全局约束

- 所有 tmux 交互通过 `emamux:tmux-run-command`；失败以 `user-error` 上抛
- 「提交 / 换行」语义映射建立在 readline 共有约定上：Enter 提交；多行内容通过 bracketed paste 协议承载，paste 内的 `\n` 由 agent 解释为 in-input 换行而非提交
- 不依赖定时等待或 TUI 内部时序
- 不针对个别 agent 做 hack 或 workaround：发送链路、识别机制、按键契约都建立在通用终端协议（bracketed paste、Enter 字节）和 POSIX 共有约定（kernel `p_comm`、`ps`）之上，代码路径不出现 `if-agent-X` 分支。`agentmux-agent-cli-commands` 等纯配置可枚举具体 agent 名，但属于数据，不是逻辑分支

## US-0010：发送纯文本到 Agent

作为用户，我希望把文本从 Emacs 发到 Agent pane 并由 Agent 提交，以便快速传递指令。

### AC-0010-0010：单行文本提交

- Given：目标已设置，文本不含换行
- When：调用发送（非暂存）
- Then：文本原样出现在 Agent 输入框并被提交为一条消息

### AC-0010-0020：多行文本作为单条消息

- Given：文本包含一个或多个 `\n`
- When：调用发送（非暂存）
- Then：整段文本作为一条消息提交；中间的换行在输入框内表现为换行而不是新消息

### AC-0010-0030：字面字符保真

- Given：文本含 `;`、`~`、`\`、tmux 格式说明符（如 `#{...}`）、ANSI 转义片段、CJK、emoji
- When：调用发送
- Then：除 `\n` 被映射为输入框换行外，其余字节按原样到达 Agent 的输入内容

### AC-0010-0040：暂存模式不提交

- Given：以暂存模式调用发送
- When：发送完成
- Then：文本进入 Agent 输入框，未提交；后续由用户编辑并手工提交

### AC-0010-0050：无时序依赖

- Given：任意文本长度或系统负载
- When：调用发送
- Then：实现不含 sleep 或等待；正确性不受 TUI render 时序或负载影响。此处「时序依赖」指 `sleep` / `sit-for` / `accept-process-output` 等显式等待，**不**包括 AC-0010-0100 要求的两次独立 tmux 调用之间天然存在的进程边界（OS / runtime 层面的事实，无须 sleep）

### AC-0010-0060：不污染共享粘贴状态

- Given：用户持有自己的 tmux paste buffer
- When：agentmux 发送任意次数
- Then：用户的 tmux paste buffer 未被覆盖或删除

### AC-0010-0070：空输入防御

- Given：空字符串
- When：调用发送（暂存）
- Then：不产生任何 tmux 操作

### AC-0010-0080：空输入 + 非暂存等价于按 Enter

- Given：空字符串
- When：调用发送（非暂存）
- Then：向 Agent pane 仅发送一次 Enter，等价于用户在 pane 内按下 Enter；不发送任何文本字节

### AC-0010-0090：行尾归一化防护

- Given：文本包含 `\r\n`（CRLF）或独立 `\r`（裸 CR，例如来自系统剪贴板、特殊 coding 的 buffer 或文件选中内容）
- When：调用发送
- Then：`\r\n` 与 `\r` 在发送前统一归一为 `\n`，再映射为输入框换行；不会出现任何 `\r` 字节被送到 Agent 导致提前提交的情况

### AC-0010-0100：发送恰好分为「写入」与「提交」两次 tmux 调用

- Given：非空文本 + 非暂存
- When：调用发送
- Then：触发恰好 2 次 tmux 命令调用——1 次写入内容（paste），1 次提交（Enter）；行数不影响调用次数；两次调用**不得 chain 在同一次 tmux 进程内**（详见历史决策「为什么写入与提交必须分两次调用」）
- 暂存模式：仅 1 次写入调用，无提交调用
- 空输入：按 AC-0010-0070 / 0080 缩减为 0 或 1 次

## US-0020：带上下文发送命令

作为用户，我希望把当前文件位置或区域作为上下文随命令一起发给 Agent，以便 Agent 理解指令所处的代码位置。

### AC-0020-0010：光标处包含文件路径与行号

- Given：光标在某文件某行，未标记区域；上下文配置包含行号
- When：调用带上下文发送
- Then：提交消息包含「路径:行号」和命令正文；路径风格来自配置

### AC-0020-0020：区域范围作为 path:start-end

- Given：有激活的区域跨若干行
- When：调用带上下文发送
- Then：提交消息包含「路径:起行-止行」和命令正文

### AC-0020-0030：可选附加代码内容

- Given：上下文配置开启「包含内容」
- When：调用带上下文发送
- Then：提交消息在位置行之后附加带围栏的代码块（当前区域或当前行）

### AC-0020-0040：非文件缓冲区纯命令发送

- Given：当前 buffer 未关联文件
- When：调用带上下文发送
- Then：仅发送命令正文，不带位置行；行为等价于 US-0010

## US-0030：发送文件引用

作为用户，我希望只把「当前位置/区域」或「当前文件路径」作为暂存内容发给 Agent，以便 Agent 定位，但不立即触发处理。

### AC-0030-0010：暂存发送 path:line

- Given：光标在某行
- When：调用发送文件引用
- Then：输入框内新增一行「路径:行号」，未提交（符合 AC-0010-0040）

### AC-0030-0020：暂存发送 path:start-end

- Given：有激活区域
- When：调用发送文件引用
- Then：输入框内新增一行「路径:起行-止行」，未提交

### AC-0030-0030：暂存发送仅路径

- Given：任意文件缓冲区
- When：调用发送仅路径
- Then：输入框内新增一行路径，未提交

### AC-0030-0040：非文件缓冲区拒绝

- Given：当前 buffer 未关联文件
- When：调用上述任一发送文件引用命令
- Then：以 `user-error` 提示，不产生 tmux 调用

## US-0040：快速回复按键

作为用户，我希望用单键直接向 Agent pane 发送常用按键，在 Agent 处于菜单或确认状态时快速响应。

### AC-0040-0010：Enter/Escape 直达

- Given：Agent 处于等待按键状态
- When：调用发送 Enter 或 Escape
- Then：相应按键立即抵达 pane，不经过文本发送链路

### AC-0040-0020：发送任意数字 0-9

- Given：Agent 显示编号菜单
- When：调用发送数字命令并输入 0-9
- Then：对应数字字符进入输入框，不触发提交

### AC-0040-0030：一键 1-4

- Given：常用选项编号
- When：按下绑定在 1-4 的快捷键
- Then：对应数字进入输入框，不触发提交

## US-0050：菜单导航模式

作为用户，我希望进入一个临时键位模式来连续操纵 Agent 的菜单，用方向键选择、回车确认、ESC 取消。

### AC-0050-0010：方向键导航

- Given：已进入菜单模式
- When：按 hjkl 或方向键
- Then：相应方向键抵达 pane；模式保持激活

### AC-0050-0020：确认与取消

- Given：已进入菜单模式
- When：按 Enter 或 Escape
- Then：对应按键抵达 pane；模式退出

### AC-0050-0030：临时文本输入

- Given：已进入菜单模式
- When：触发「Input」条目并输入一段文本
- Then：文本发送到 pane（非暂存）

## US-0060：目标设置

作为用户，我希望选择 tmux 中哪个 pane 作为 Agent pane，以便把命令发对地方。

### AC-0060-0010：交互选择 session/window/pane

- Given：tmux 有多个 session/window/pane
- When：调用设置目标命令
- Then：提供交互选择器；选中的 pane 被持久化为当前会话目标

### AC-0060-0020：未设置时的发送阻断

- Given：目标未设置
- When：调用任何发送命令
- Then：先触发目标设置；若用户取消，以 `user-error` 中止并提示使用 `M-x agentmux-set-target`

## US-0070：目标选择的智能默认

作为在 tmux 中运行 Emacs 的用户，我希望首次或手动选择目标时系统给出与我当前上下文匹配的默认值，避免每次都手翻 session/window 列表，也避免误选到 Emacs 自己所在的 pane。

### AC-0070-0010：默认 window 指向 Emacs 所在 window

- Given：Emacs 在 tmux 中运行，当前 Emacs 所在 window 已知
- When：目标选择界面展开 window 候选
- Then：Emacs 所在 window 是默认候选（或排在首位）

### AC-0070-0020：候选 pane 排除 Emacs 自身

- Given：候选 pane 列表中含 Emacs 自身所在 pane
- When：生成候选列表
- Then：Emacs 自身 pane 不出现在候选列表中（防自送，且避免视觉干扰）

### AC-0070-0030：两个或更多候选 pane 时优先 Agent pane

- Given：同一 window 排除 Emacs 自身后仍有两个或更多 pane
- When：生成默认 pane 候选
- Then：优先选中运行 Agent CLI（如 Claude Code）的 pane 作为默认；其余 pane 退居其后

### AC-0070-0040：Emacs 不在 tmux 中时的回退

- Given：Emacs 未在 tmux 中运行（`TMUX_PANE` 环境变量缺失或为空）
- When：调用目标设置
- Then：跳过「Emacs 所在 window / pane」相关的智能排序与排除，但其余排序规则继续生效——tmux-active 优先（active window / active pane 仍排在前列），且 AC-0070-0030 的 Agent CLI 优先排序仍对所有候选 pane 生效（与 `TMUX_PANE` 无依赖）

### AC-0070-0050：显示顺序的稳定性

- Given：列表候选多于一项
- When：候选被排序
- Then：默认值或最高优先级项位于列表前端；同级项顺序在同一会话内稳定，不随发送次数变化

### AC-0070-0060：tmux 元数据查询失败 fail-fast

- Given：`TMUX_PANE` 已设但 tmux 元数据查询失败（例如该 pane 已被销毁、tmux server 不可用）
- When：调用目标设置
- Then：以 `user-error` 中止并指出原因；不静默退化为「Emacs 不在 tmux」的回退路径

### AC-0070-0070：候选顺序对完成框架透明

- Given：使用任意 minibuffer 完成框架（vertico / ivy / 默认 completing-read 等）
- When：弹出候选选择 UI
- Then：UI 按 agentmux 提供的顺序展示候选，不被框架按字母序、长度、历史等规则重排

## US-0080：入口与菜单配置

作为用户，我希望通过一个 transient 入口菜单切换发送选项并访问所有命令。

### AC-0080-0010：入口菜单

- Given：绑定了入口命令
- When：调用入口
- Then：显示 Options / Send / Quick / Reply / Mode / Action 分组；每次打开重置选项默认值

### AC-0080-0020：可切换选项

- Given：入口菜单显示
- When：切换路径风格、是否含行号、是否含内容
- Then：下次发送使用新的选项

### AC-0080-0030：路径风格候选依赖项目结构

- Given：Emacs 项目根与 Git 根不同
- When：切换路径风格
- Then：候选包括 `project`；否则候选仅 `git/absolute/filename`

## 非功能约束

- 所有自定义变量和函数统一 `agentmux-` / `agentmux--` 前缀；`agentmux-` 公开，`agentmux--` 私有
- 配置通过 `defcustom`，归入 `agentmux` 自定义组，支持 `M-x customize-group RET agentmux`
- 包覆盖 `emamux` 的部分显示函数（via `advice-add`），卸载时通过 `agentmux-unload-function` 清除
- 代码遵循 `.el` 通用约定：`lexical-binding: t`；`use-package` 配置；内置/site-lisp 不 `:ensure`

## ADR

### ADR-001: Agent CLI pane 通过 `ps` 进程树识别

- Status: Accepted
- Context: 需要从 tmux pane 识别其内运行的 agent CLI，作为目标选择的智能默认依据。
- Decision: 以 tmux `pane_pid` 为根，调用 `ps -axo pid=,ppid=,comm=` 取全量进程表，BFS 后裔，匹配 `comm` basename 与 `agentmux-agent-cli-commands`。
- Consequences:
  - 读 kernel `p_comm`（`exec` 后不可变），不受 `process.title` 改写影响
  - 自动穿透 shell / wrapper / supervisor 中间进程
  - macOS 与 Linux `ps` 调用签名一致
  - 每次目标选择多一次 `ps` 调用（约几十毫秒）；对系统 `ps` 可执行文件存在隐性依赖
- Rejected alternatives:
  - tmux `pane_current_command` / Emacs `process-attributes` 的 `comm`：在 macOS 读 `pbi_name`，受 `setproctitle` 污染（Claude Code 等 Node TUI 会把 `process.title` 改写为版本号）

### ADR-002: 候选顺序对 minibuffer 完成框架透明

- Status: Accepted
- Context: vertico / ivy 等完成框架默认按字母 / 历史 / 长度重排候选，会打乱 agentmux 计算的智能排序。
- Decision: 所有 `read-parameter-*` 命令通过自定义 completion table 的 `metadata` 把 `display-sort-function` 与 `cycle-sort-function` 设为 `identity`，强制保留传入顺序。

### ADR-003: Emacs 自身 pane 从候选列表移除

- Status: Accepted
- Context: 默认目标不应指向 Emacs 自己（防自送）；视觉上不应出现 Emacs pane 干扰选择。
- Decision: 在生成候选列表时 `seq-remove` Emacs pane（依据 `TMUX_PANE`），而非排末位。
- Consequences:
  - 自送在 UI 层不可达
  - 当 Emacs 是 window 内唯一 pane 时候选列表为空，`agentmux--emamux-read-parameter-pane` 抛 `user-error`

### ADR-004: 失败语义 fail-fast，禁止 fallback

- Status: Accepted
- Decision: 遵守 `site-lisp/CLAUDE.md`「失败语义」约束（fail-fast，禁止 `ignore-errors` / 空值兜底，`condition-case` 仅用于透传或带业务上下文的 user-error 包装）
- Project-specific exception: 唯一允许的「回退」是 AC-0070-0040「Emacs 不在 tmux」，由 `TMUX_PANE` 是否为 nil 显式条件区分（不属于异常捕获）

### ADR-005: 发送链路用命名 paste-buffer，写入与提交分两次 tmux 调用

- Status: Accepted
- Context: 需要把多行内容作为单条消息送进 agent 输入框并提交。约束：不得污染用户默认 paste stack；不得违反 AC-0010-0100「至多 2 次 tmux 调用」；不得依赖 sleep（AC-0010-0050）；不得让 React Ink 的 `usePaste`/`useInput` race 吞掉提交。
- Decision:
  - 写入：`set-buffer -b agentmux-private DATA ; paste-buffer -p -d -b agentmux-private -t T` 在同一次 `process-file` 内 chain；命名 buffer 用 `-d` 即时删除
  - 提交：单独一次 `process-file` 跑 `send-keys -t T Enter`
  - 写入与提交**不得 chain 在同一次 tmux 调用**
- Consequences:
  - 每次发送恰好 2 次 `process-file`（暂存 1 次，空输入 0 或 1 次），与行数无关
  - bracketed paste（`-p`）让 agent 把整段当 in-input 字节，多行映射为输入框换行而非多消息
  - 命名 buffer + `-d` 不污染默认 paste stack
  - 两次 `process-file` 之间的 emacs lisp 调度 + 第二次 fork 的进程边界足以让 React 完成 `usePaste` setState commit，`useInput` 拿 Enter 时正确捕获 input；属于 OS / runtime 的天然边界，不是 sleep
- Rejected alternatives:
  - chain 1-call（`set-buffer ; paste-buffer ; send-keys Enter`）：tmux 在同一 client tick 连续写字节流，无间隙让 React commit，`useInput` 提交空字符串，paste 内容残留输入框
  - 每行一次 `send-keys -l LINE` + 行间 `M-Enter`：违反 AC-0010-0100，且 argv 体积随行数线性增长
  - `send-keys -l` body 内联 `\e\r` 字面字节：ESC 字节让 Ink 输入状态机进入 escape sequence 等待，吞掉后续 Enter
  - `paste-buffer` 不带 `-p`：默认把 `\n` 转 `\r`，每行触发提交导致拆分
