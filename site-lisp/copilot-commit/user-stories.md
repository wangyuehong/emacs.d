# User Stories

## US-0010: 生成 commit message

作为开发者,我希望在 commit buffer 中通过命令生成符合 Conventional Commits 规范的 commit message,以便快速完成 git commit 流程。

### 验收标准

#### AC-0010-0010: 正常生成

- Given: git 仓库有 staged changes,copilot LSP 连接正常
- When: 在 commit buffer 中执行 `copilot-commit-insert-message`
- Then: 基于 staged diff 和 git status 流式生成 commit message,实时显示在 buffer 中

#### AC-0010-0020: 流式实时显示

- Given: 生成命令已触发
- When: 服务端返回流式 chunk
- Then: commit buffer 中用户输入区域(第一个 `#` 注释行之前)被替换为当前累积的生成内容,`#` 注释行、scissor 线、verbose diff 等完全不受影响

#### AC-0010-0030: 覆盖已有内容

- Given: commit buffer 中已有用户手写或之前生成的内容
- When: 执行生成命令
- Then: 用户输入区域的内容被直接覆盖,无需确认

#### AC-0010-0040: 无 staged changes

- Given: git 仓库没有 staged changes
- When: 执行生成命令
- Then: 通过 `message` 提示 "no staged changes",不发起生成请求

#### AC-0010-0050: LSP 未连接

- Given: copilot LSP 连接未建立
- When: 执行生成命令
- Then: 通过 `message` 提示 "LSP connection is not active"

#### AC-0010-0060: 生成过程中重复调用

- Given: 正在流式生成中
- When: 再次执行生成命令
- Then: 通过 `message` 提示 "generation already in progress",不发起新请求

## US-0020: 重新生成 commit message

作为开发者,我希望对已生成的 commit message 补充指示并重新生成,以便迭代优化 commit message 质量。

### 验收标准

#### AC-0020-0010: 正常重新生成

- Given: 已有一次成功生成的 commit message(历史记录存在)
- When: 执行 `copilot-commit-regenerate-message`
- Then: 弹出 minibuffer 输入额外指示,销毁旧会话,在新会话的 turns 中包含完整历史和新指示,流式替换当前内容

#### AC-0020-0020: 无前次生成

- Given: 没有进行过生成(无历史记录)
- When: 执行重新生成命令
- Then: 回退为首次生成行为(等同于执行 `copilot-commit-insert-message`)

#### AC-0020-0030: 空指示

- Given: 已有成功生成的 commit message
- When: 执行重新生成命令,输入内容为空
- Then: 使用默认指示 "Please regenerate" 发送请求

## US-0030: 取消生成

作为开发者,我希望在生成耗时较长或改变主意时中断当前生成,以便不必等待完成。

### 验收标准

#### AC-0030-0010: 正常取消

- Given: 正在流式生成中
- When: 执行 `copilot-commit-cancel`
- Then: 停止流式插入,销毁当前会话,清除 buffer 用户输入区域的进度文字,通过 `message` 提示 "cancelled"

#### AC-0030-0020: 无生成进行中

- Given: 没有正在进行的生成
- When: 执行取消命令
- Then: 通过 `message` 提示 "no generation in progress"

## US-0040: 可定制 prompt 后缀

作为有特定规则的开发者,我希望在生成 commit message 的 prompt 末尾追加自定义内容,以便添加语言指令或其他规则。

### 验收标准

#### AC-0040-0010: 字符串后缀

- Given: `copilot-commit-prompt-suffix` 设置为字符串
- When: 执行生成命令
- Then: 该字符串追加到 prompt 末尾

#### AC-0040-0020: 函数后缀

- Given: `copilot-commit-prompt-suffix` 设置为函数
- When: 执行生成命令
- Then: 调用该函数,将返回值追加到 prompt 末尾

#### AC-0040-0030: 默认无后缀

- Given: `copilot-commit-prompt-suffix` 为默认值 `""`
- When: 执行生成命令
- Then: 不附加任何内容

## US-0050: 可定制 prompt

作为有特定 commit 规范的开发者,我希望自定义生成 commit message 的系统 prompt,以便适配项目的 commit 规范。

### 验收标准

#### AC-0050-0010: 默认 prompt

- Given: 未自定义 `copilot-commit-prompt`
- When: 执行生成命令
- Then: 使用内置的 Conventional Commits 格式 prompt

#### AC-0050-0020: 自定义 prompt

- Given: 通过 `setopt` 或 Emacs customize 设置了自定义 `copilot-commit-prompt`
- When: 执行生成命令
- Then: 使用用户自定义的 prompt 内容

## US-0060: 可定制模型

作为开发者,我希望指定生成 commit message 使用的 AI 模型,以便在速度和质量之间权衡。

### 验收标准

#### AC-0060-0010: 指定模型

- Given: `copilot-commit-model` 配置为非 nil 的模型 ID
- When: 执行生成命令
- Then: `conversation/create` payload 中包含 `:model` 字段,使用指定模型生成

#### AC-0060-0020: 未指定模型

- Given: `copilot-commit-model` 为 nil
- When: 执行生成命令
- Then: `conversation/create` payload 中不包含 `:model` 字段,由 copilot LSP server 决定使用的模型

## US-0070: 依赖兼容性检查

作为升级 copilot 包的用户,我希望 `copilot-commit` 能优雅地处理缺失或改名的内部 API,以便 Emacs 启动不中断且能获得清晰的诊断信息。

### 验收标准

#### AC-0070-0010: 依赖完整

- Given: copilot 包提供所有必需的内部符号
- When: `copilot-commit` 加载
- Then: 所有功能正常工作,不输出警告

#### AC-0070-0020: 加载时依赖缺失

- Given: copilot 包更新后部分必需的内部符号被移除或改名
- When: `copilot-commit` 加载
- Then: 通过 `display-warning` 输出 `:error` 级别警告,列出缺失符号;禁用所有功能,不注册通知/请求处理器

#### AC-0070-0030: 禁用后调用命令

- Given: `copilot-commit` 因依赖缺失已禁用
- When: 用户调用任何 `copilot-commit` 交互命令
- Then: 通过 `message` 提示 "disabled due to missing copilot internals"

## US-0080: 大 diff 分块生成

作为开发者,我希望大型 diff 也能成功生成 commit message,以便不受 LSP token 限制影响。

### 验收标准

#### AC-0080-0010: 大 diff 自动分块

- Given: staged diff 字符数超过 `copilot-commit-chunk-threshold`(默认 448800)
- When: 执行 `copilot-commit-insert-message`
- Then: diff 按文件边界拆分为多个 chunk,逐个通过独立 conversation summarize,最终合并 summaries 生成 commit message

#### AC-0080-0020: 进度显示

- Given: 分块处理正在进行
- When: 命令触发时及每个 chunk 完成时
- Then: commit buffer 用户输入区域初始显示 `Analyzing changes (0/M)...`,每个 chunk 完成后更新为 `Analyzing changes (N/M)...`,所有 chunk 完成后显示 `Generating commit message...` 并流式替换为最终内容

#### AC-0080-0030: 小 diff 不受影响

- Given: staged diff 字符数未超过 `copilot-commit-chunk-threshold`
- When: 执行 `copilot-commit-insert-message`
- Then: 走现有单轮路径,行为不变

#### AC-0080-0040: 分块后 regenerate

- Given: 分块生成已完成,summaries 缓存在 buffer-local 变量中
- When: 执行 `copilot-commit-regenerate-message`
- Then: 使用缓存的 summaries 重建 final prompt,不重复分块过程

#### AC-0080-0050: 按文件边界拆分

- Given: diff 包含多个文件的变更
- When: 执行分块拆分
- Then: 以 `diff --git` 行为边界拆分为独立文件 diff,拆分后拼接与原 diff 完全一致,无内容丢失

#### AC-0080-0060: 贪心合并

- Given: 拆分后的文件 diff 列表
- When: 执行贪心合并
- Then: 依次累积追加,累积长度超过 threshold 时开新 chunk,等于 threshold 时合并到当前 chunk

#### AC-0080-0070: 单文件超大按 hunk 再拆

- Given: 某个文件的 diff 字符数超过 threshold 且包含多个 hunk
- When: 贪心合并处理该文件
- Then: 先 flush 已有累积器,按 `@@` hunk 边界再拆分,每个子 chunk 保留原始文件 header

#### AC-0080-0080: 不可再拆的超大 hunk

- Given: 某个文件的 diff 超过 threshold 且仅含 1 个 hunk
- When: 贪心合并处理该文件
- Then: 该 hunk 作为独立 chunk 原样保留,不报错不截断

#### AC-0080-0090: 拆分后仅一个 chunk

- Given: diff 超过 threshold 但拆分合并后仅产生 1 个 chunk
- When: 执行分块生成
- Then: 退化为单轮路径,行为与小 diff 一致

#### AC-0080-0100: 空 diff

- Given: diff 为空字符串
- When: 执行分块拆分
- Then: 返回空列表

#### AC-0080-0110: hunk 拆分的 header 保留

- Given: 单文件 diff 按 hunk 拆分为多个子 chunk
- When: 检查每个子 chunk 内容
- Then: 每个子 chunk 都以原始文件的 `diff --git` header 开头

## US-0090: 动态 chunk threshold

作为开发者,我希望 chunk threshold 根据模型的 token limit 自动计算,以便充分利用模型容量。

### 验收标准

#### AC-0090-0010: 有缓存时使用计算值

- Given: 缓存存在对应模型的 token limit
- When: 获取 chunk threshold
- Then: threshold = max(10000, (floor(max_tokens * 0.9) - 3000) * 4)

#### AC-0090-0020: 无缓存时使用默认值

- Given: 无缓存(未命中或已过期)
- When: 获取 chunk threshold
- Then: 使用 `copilot-commit-chunk-threshold` 默认值,同时触发 probe 请求;过期缓存用旧值兜底

#### AC-0090-0030: probe 请求

- Given: 首次使用,无缓存
- When: 触发 probe
- Then: 发送最小 payload 的 `conversation/create`,从 progress report 的 `contextSize.totalTokenLimit` 提取并缓存,请求完成后 destroy conversation

#### AC-0090-0040: progress report 按需更新缓存

- Given: 正常生成的 progress report 事件
- When: 事件包含 `contextSize.totalTokenLimit` 且缓存不存在或已过期
- Then: 写入缓存(含时间戳),下次使用时生效;缓存未过期时不更新

#### AC-0090-0050: 缓存 TTL

- Given: 缓存条目存在
- When: 条目写入时间距今超过 `copilot-commit-cache-ttl`(默认 48 小时)
- Then: 视为过期,使用旧值兜底并触发 probe 刷新

## US-0100: 并发 summarize

作为开发者,我希望分块 summarize 请求并发发送,以便缩短大 diff 的等待时间。

### 验收标准

#### AC-0100-0010: 并发控制

- Given: 大 diff 被拆分为多个 chunk
- When: 开始分块 summarize
- Then: 同时 inflight 的 summarize 请求不超过 `copilot-commit-chunk-concurrency`(默认 3)

#### AC-0100-0020: 顺序保证

- Given: 多个 chunk 并发 summarize
- When: chunk 以任意顺序完成
- Then: summaries 按 chunk index 顺序排列,不受完成顺序影响

#### AC-0100-0030: 窗口滑动

- Given: 某个 chunk 完成
- When: 还有未发送的 chunk
- Then: dispatch 下一个待发送的 chunk

#### AC-0100-0040: 全部完成后触发 final

- Given: 所有 chunk 的 summarize 完成
- When: 最后一个 chunk 完成
- Then: 发送 final prompt 生成 commit message

#### AC-0100-0050: 进度显示

- Given: 分块处理正在进行
- When: 每个 chunk 完成时
- Then: 显示 `Analyzing changes (completed/total)...`

#### AC-0100-0060: 取消清理

- Given: 并发 summarize 正在进行
- When: 用户执行取消
- Then: 清除所有 inflight 请求和状态

## US-0110: UI 进度反馈

作为开发者,我希望在命令执行后 buffer 立即有视觉反馈,以便知道生成正在进行。

### 验收标准

#### AC-0110-0010: 单轮生成即时反馈

- Given: 执行 `copilot-commit-insert-message`,diff 未超过 threshold
- When: 命令触发后,流式内容到达前
- Then: buffer 用户输入区域显示 `Generating...`,minibuffer 显示 "generating..."

#### AC-0110-0020: 分块生成即时反馈

- Given: 执行 `copilot-commit-insert-message`,diff 超过 threshold 且分块 > 1
- When: 命令触发后
- Then: buffer 用户输入区域显示 `Analyzing changes (0/N)...`,minibuffer 显示 "analyzing large diff (N parts)..."

#### AC-0110-0030: regenerate 即时反馈

- Given: 执行 `copilot-commit-regenerate-message`
- When: 命令触发后,流式内容到达前
- Then: buffer 用户输入区域显示 `Regenerating...`,minibuffer 显示 "regenerating..."

#### AC-0110-0040: 生成完成后清除 minibuffer

- Given: 生成正在进行,minibuffer 显示进度消息
- When: 生成完成(收到 "end" 事件)
- Then: minibuffer 进度消息被清除
