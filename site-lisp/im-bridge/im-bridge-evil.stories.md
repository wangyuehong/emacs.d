# im-bridge-evil 用户故事

## US-0010: Evil 模式输入法自动切换

作为 Evil 用户，我希望在退出 insert state 时自动切换到英文输入法，进入 insert state 时自动恢复之前的输入法，以便在 normal state 下流畅使用 Vim 快捷键。

### 验收标准

#### AC-0010-0010: 退出 insert state 时保存并切换系统输入法

- Given: 用户处于 insert state，系统输入法为中文
- When: 用户退出 insert state（如按 ESC）
- Then: 系统输入法被保存到 buffer-local 变量，系统输入法切换为英文

#### AC-0010-0020: 退出 insert state 时保存并关闭 Emacs 输入法

- Given: 用户处于 insert state，Emacs 输入法已激活
- When: 用户退出 insert state
- Then: Emacs 输入法被保存到 buffer-local 变量，Emacs 输入法被关闭

#### AC-0010-0030: 进入 insert state 时恢复 Emacs 输入法优先

- Given: 用户处于 normal state，之前保存了 Emacs 输入法
- When: 用户进入 insert state
- Then: Emacs 输入法被恢复，系统输入法被确保为英文

#### AC-0010-0035: Emacs 输入法恢复时丢弃系统输入法状态

- Given: 用户处于 normal state，之前同时保存了 Emacs 输入法和系统输入法
- When: 用户进入 insert state
- Then: Emacs 输入法被恢复，系统输入法设为英文，保存的系统输入法状态被丢弃

#### AC-0010-0040: 进入 insert state 时恢复系统输入法

- Given: 用户处于 normal state，之前保存了系统输入法（无 Emacs 输入法）
- When: 用户进入 insert state
- Then: 系统输入法被恢复为之前保存的状态

#### AC-0010-0050: 退出 insert state 时系统已是英文则不保存

- Given: 用户处于 insert state，系统输入法已是英文
- When: 用户退出 insert state
- Then: 系统输入法状态不被保存（保持 nil），无切换操作

#### AC-0010-0060: 进入 insert state 时无保存状态则不操作

- Given: 用户处于 normal state，没有保存的输入法状态
- When: 用户进入 insert state
- Then: 输入法状态保持不变

## US-0020: 输入法状态 buffer 隔离

作为用户，我希望每个 buffer 独立保存输入法状态（包括系统输入法和 Emacs 输入法），以便在不同 buffer 间切换时各自保持输入法上下文。

### 验收标准

#### AC-0020-0010: 不同 buffer 系统输入法状态独立

- Given: buffer A 保存了中文系统输入法状态，buffer B 保存了日文系统输入法状态
- When: 用户在 buffer A 进入 insert state
- Then: 恢复中文系统输入法，不受 buffer B 状态影响

#### AC-0020-0020: 不同 buffer Emacs 输入法状态独立

- Given: buffer A 保存了 pyim 输入法状态，buffer B 保存了 rime 输入法状态
- When: 用户在 buffer A 进入 insert state
- Then: 恢复 pyim 输入法，不受 buffer B 状态影响

#### AC-0020-0030: 新 buffer 无保存状态

- Given: 用户打开一个新 buffer
- When: 用户进入 insert state
- Then: 无输入法恢复操作（保存状态为 nil）

## US-0030: 全局模式启用与禁用

作为用户，我希望通过 `imb-evil-mode` 全局控制输入法桥接功能的启用与禁用。

### 验收标准

#### AC-0030-0010: 启用全局模式

- Given: `imb-evil-mode` 未启用
- When: 用户执行 `(imb-evil-mode 1)`
- Then: Evil 的 insert state 钩子被安装，输入法桥接功能生效

#### AC-0030-0020: 禁用全局模式

- Given: `imb-evil-mode` 已启用
- When: 用户执行 `(imb-evil-mode -1)`
- Then: Evil 的 insert state 钩子被移除，输入法桥接功能停止

#### AC-0030-0030: 延迟加载 Evil 兼容

- Given: Evil 尚未加载
- When: 用户启用 `imb-evil-mode`
- Then: 钩子安装被延迟到 Evil 加载后执行
