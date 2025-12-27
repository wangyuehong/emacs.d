# im-bridge-core 用户故事

## US-0010: CLI 工具和输入法 ID 可配置

作为用户，我希望能自定义 CLI 工具和英文输入法 ID，以便适配不同的系统环境。

### 验收标准

#### AC-0010-0010: 自定义 CLI 工具

- Given: 用户设置 `imb-cli-command` 为 `"im-select"`
- When: 系统执行输入法查询或切换
- Then: 使用 `im-select` 命令而非默认的 `macism`

#### AC-0010-0020: 自定义英文输入法 ID

- Given: 用户设置 `imb-english-id` 为 `"com.apple.keylayout.US"`
- When: 系统切换到英文输入法
- Then: 切换到 `com.apple.keylayout.US` 而非默认值

#### AC-0010-0030: CLI 工具不存在时报错

- Given: `imb-cli-command` 配置的命令不存在
- When: 系统查询当前输入法
- Then: 抛出错误，提示命令未找到
