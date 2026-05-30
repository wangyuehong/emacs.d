# Handoff

- 日期: 2026-05-30 11:03
- 分支: master

> 致接手 agent：
>
> 这是一份由前一个 agent 生成的工作交接文档。你无法访问前一个会话的上下文，本文档是你了解当前工作状态的唯一来源。
>
> 请按以下方式使用：
>
> 1. 先完整阅读本文档，建立全局理解
> 2. 按「接续指南」中的第一步开始工作
> 3. 开始工作后删除本文件
>
> 如果文档中有不清楚的地方，请向用户确认，不要猜测。

## 目标与方向

当前任务是给 `code-ref` 和 `agentmux` 的带内容代码引用增加自动缩略能力，避免长 region 完整进入剪贴板或 agent 输入。默认行为应是自动缩略：阈值默认 3 行，阈值可配置但不得低于 3；内容行数不超过阈值时保留完整内容，超过阈值时仅保留第一行、最后一行，并在中间插入 `[... omitted N lines ...]`。

同时要保持位置引用覆盖原始完整 region，不因内容缩略改变；带内容复制或发送前需要保存 file-visiting buffer，使行号基于保存后的 buffer。用户最新补充要求：修复所有 `checkdoc` 警告，并同步更新 spec 中关于阈值默认值和校验规则的表述。

## 现状

已完成并验证的工作:

- `code-ref` 已实现共享内容格式化能力：`cref-content-format` 支持 `auto` / `full`，默认 `auto`；`cref-content-auto-compact-threshold` 默认 3，custom type 与 safe-local 校验要求至少 3。
- `code-ref` 运行时已用 `(max 3 cref-content-auto-compact-threshold)` 钳制阈值，避免用户用 `setq` 绕过 custom 校验。
- `code-ref` 的保存顺序已提前到计算 bounds、path 和 location 之前。
- `agentmux` 已新增 `agentmux-context-content-format`，默认 `auto`，并调用 `code-ref-core` 的统一围栏/缩略格式化函数。
- `agentmux--context-parts` 已在 file-visiting buffer 中先保存再生成 bounds/location/content；非文件 buffer 分支仍直接返回 nil，不保存、不生成内容。
- `SPEC.md` 已同步新增自动缩略、保存顺序、非文件 buffer 不保存不生成内容等验收标准；用户最后要求的默认阈值和最低校验规则也已补入两个 spec。
- 已开始修复 `checkdoc` 警告：`code-ref-core.el` 的 `kill-ring` docstring 和部分 `agentmux.el` docstring / message 已改，但尚未重新跑完验证。

已验证结果:

- `site-lisp/code-ref`: `make test` 已通过，最近一次为 50 tests 全部通过。
- `site-lisp/agentmux`: `make test` 在 sandbox 内会因为既有测试调用真实 `ps` 被拒绝；使用 escalation 在 sandbox 外已通过，63 tests 全部通过。
- `make checkdoc` 在两个包中均返回退出码 0，但此前仍有警告。用户要求修复所有警告后需要重新跑。
- `git diff --check` 此前通过；修复 checkdoc 后应重跑。

已变更的文件:

- `site-lisp/code-ref/SPEC.md` - 新增自动缩略、阈值默认值、最低校验、长内容缩略和位置范围不变的验收标准。
- `site-lisp/code-ref/code-ref-core.el` - 新增内容格式 defcustom、阈值 defcustom、运行时阈值下限钳制、共享内容格式化函数，并让围栏基于最终输出文本计算。
- `site-lisp/code-ref/code-ref.el` - 将保存动作提前到 bounds/path/location 生成之前。
- `site-lisp/code-ref/code-ref-test.el` - 新增阈值边界、custom 校验、运行时最低阈值、缩略输出、full 模式、保存顺序等测试。
- `site-lisp/agentmux/SPEC.md` - 新增上下文内容与 `code-ref` 一致、保存后再生成上下文、非文件 buffer 不保存不生成内容的验收标准；已写明默认阈值 3 和最低值 3。
- `site-lisp/agentmux/agentmux.el` - 新增上下文内容格式配置，复用 `code-ref-core` 内容格式化，并在上下文生成前保存。
- `site-lisp/agentmux/agentmux-test.el` - 新增上下文内容缩略、关闭内容行为不变、非文件 buffer 无保存/格式化、保存后行号等测试；同步了 `agentmux--tmux-call-or-error` 错误消息测试。

## 接续指南

第一步: 从项目根运行 `git status --short` 和 `git diff --check`，确认只存在上述任务相关变更且无空白错误。

待完成:

- 完成用户要求的“fix 所有警报”：重新运行 `make checkdoc` in `site-lisp/code-ref` 和 `site-lisp/agentmux`，修复所有 warning，而不只修本次新增 warning。
- 修完 `checkdoc` 后重跑 `make test` in `site-lisp/code-ref`。
- 修完 `checkdoc` 后重跑 `make test` in `site-lisp/agentmux`。如果 sandbox 内仍因 `ps` 报 `Doing vfork Operation not permitted`，按审批流程用 escalation 重跑；之前 sandbox 外已证明可通过。
- 复核 `site-lisp/code-ref/SPEC.md` 和 `site-lisp/agentmux/SPEC.md`，确认阈值默认值 3、最低值 3、超过当前阈值缩略这三点在 spec 中一致。
- 最后做一次语义自查：确认 `auto` 模式按最终输出计算围栏长度；`full` 模式不缩略；缩略只影响代码围栏内容，不影响 location 范围；非文件 buffer 仍只发送命令。

建议执行顺序:

1. 先修 `checkdoc`，因为它可能要求改 message 文本，影响测试断言。
2. 再跑 `code-ref` 和 `agentmux` 测试。
3. 最后查看 `git diff`，避免因 autocorrect 或 checkdoc 修复引入无关文档变化。

## 风险与参考

已知风险:

- `cref-content-auto-compact-threshold` 的 custom 校验不足以覆盖 `(setq cref-content-auto-compact-threshold 2)`，所以运行时必须保留 `(max 3 ...)` 钳制。不要删掉该逻辑。
- `split-string` 对没有尾随换行的常规 region 行数符合预期。若未来要处理尾随换行语义，需要先补 spec，不要在本任务中扩大范围。
- `agentmux` 的 `make test` 在 sandbox 内可能失败，不代表代码失败；失败点是既有 pane 排序测试中的真实 `ps` 调用。
- 修 `checkdoc` message 首字母警告时，注意同步 `agentmux-test.el` 里对错误消息的断言。

已排除的方向:

- 没有在 `agentmux` 里复制缩略算法；算法集中在 `code-ref-core`，避免双写。
- 没有给 transient 菜单新增内容格式切换项；计划要求保留现有 `Content: yes/no`，格式通过 customize 配置。
- 没有让 `cref-content-auto-compact-threshold` 允许低于 3；用户明确要求最低值是 3。

参考资料:

- `site-lisp/code-ref/SPEC.md`
- `site-lisp/code-ref/code-ref-core.el`
- `site-lisp/code-ref/code-ref-test.el`
- `site-lisp/agentmux/SPEC.md`
- `site-lisp/agentmux/agentmux.el`
- `site-lisp/agentmux/agentmux-test.el`
