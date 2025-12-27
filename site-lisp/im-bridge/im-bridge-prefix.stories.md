# im-bridge-prefix 用户故事

## US-0010: 前缀键输入法自动切换

作为用户，我希望在按下前缀键后自动切换到英文输入法，以便后续按键不被中文/日文输入法拦截。

### 验收标准

#### AC-0010-0010: 按下全局前缀键时切换

- Given: 系统输入法为中文
- When: 用户按下 C-x 或 C-c
- Then: 系统输入法切换为英文

#### AC-0010-0020: 按下 Evil leader 键时切换

- Given: 系统输入法为中文，SPC 作为 leader 键生效
- When: 用户按下 SPC
- Then: 系统输入法切换为英文

#### AC-0010-0030: 普通输入时不触发

- Given: 用户处于 Evil insert state
- When: 用户按下 SPC 进行普通字符输入
- Then: 不触发输入法切换

#### AC-0010-0040: 已是英文时不重复切换

- Given: 系统输入法已是英文
- When: 用户按下任何前缀键
- Then: 不调用 CLI 切换

#### AC-0010-0050: 快速连续按键时节流

- Given: 用户快速连续按多个前缀键
- When: 两次按键间隔小于配置的时间间隔
- Then: 跳过 CLI 调用

## US-0020: 全局模式启用与禁用

作为用户，我希望通过 `imb-prefix-mode` 全局控制前缀键输入法切换功能的启用与禁用。

### 验收标准

#### AC-0020-0010: 启用全局模式

- Given: `imb-prefix-mode` 未启用
- When: 用户执行 `(imb-prefix-mode 1)`
- Then: 前缀键切换功能生效

#### AC-0020-0020: 禁用全局模式

- Given: `imb-prefix-mode` 已启用
- When: 用户执行 `(imb-prefix-mode -1)`
- Then: 前缀键切换功能停止

#### AC-0020-0030: CLI 不可用时拒绝启用

- Given: CLI 命令不存在
- When: 用户执行 `(imb-prefix-mode 1)`
- Then: 模式不启用，显示警告消息
