# code-ref SPEC

本文件是 code-ref 包的行为契约，是代码变更的权威参照。所有修改必须保证 spec、代码、测试三者一致。

## 术语

- 源文件：当前 buffer 关联的文件
  - file-visiting buffer：取 `buffer-file-name`
  - dired buffer：取光标所在条目；若光标不在任何条目上（目录头、空行等），取当前 dired 子目录本身
- Style：路径格式化风格，包括 `absolute`（绝对路径）、`git`（相对 git 根）、`filename`（仅文件名）、`project`（相对 project 根）、`display`（git 优先，无 git 则绝对）
- 位置串：形如 `@<path>#L<n>` 或 `@<path>#L<n>-L<m>` 的行号标注片段

## 全局约束

- 路径解析失败以 `user-error` 终止并指明原因；禁止用 `condition-case` 把异常压成 buffer 名等默认值
- dired-mode 与 file-visiting buffer 走同一条 style 格式化链路，行为差异仅在「源文件如何取到」这一步
- 源文件路径在格式化前统一经过 `file-truename` 规整
- 写入剪贴板：优先 `xclip-set-selection`；不可用时回退到 `kill-ring`，消息中以「to kill-ring」后缀标明。此为显式条件分支，不是异常兜底

## US-0010：复制当前 buffer 关联的路径

作为用户，我希望用一个命令复制当前 buffer 对应的路径，以便粘贴到消息、评论或提交信息中。

### AC-0010-0010：file-visiting buffer 绝对路径

- Given：buffer 正在访问一个文件
- When：请求 `absolute` 格式
- Then：剪贴板得到该文件的绝对路径

### AC-0010-0020：file-visiting buffer git 相对路径

- Given：buffer 正在访问一个位于 git 仓库内的文件
- When：请求 `git` 格式
- Then：剪贴板得到相对于 git 根目录的路径

### AC-0010-0030：file-visiting buffer 仅文件名

- Given：buffer 正在访问一个文件
- When：请求 `filename` 格式
- Then：剪贴板得到仅含文件名的字符串，不含任何目录部分

### AC-0010-0040：dired 光标在条目上

- Given：当前 buffer 处于 dired-mode，光标位于某个文件或子目录条目
- When：请求任一 style
- Then：剪贴板得到该条目对应路径，按 style 格式化

### AC-0010-0050：dired 光标不在条目上

- Given：当前 buffer 处于 dired-mode，光标不在任何条目上（目录头、空行等）
- When：请求任一 style
- Then：剪贴板得到当前 dired 子目录本身的路径，按 style 格式化

### AC-0010-0060：非文件非 dired buffer

- Given：当前 buffer 未访问文件且不是 dired-mode（如 `*scratch*`、临时 buffer）
- When：请求 `absolute` / `git` / `filename` / `project` 任一 style
- Then：操作以 `user-error` 终止，剪贴板与 `kill-ring` 均不被写入

### AC-0010-0070：`git` style 不在 git 仓库

- Given：源文件存在但不位于任何 git 仓库下
- When：请求 `git` 格式
- Then：操作以 `user-error` 终止，剪贴板与 `kill-ring` 均不被写入

### AC-0010-0080：`display` style 兜底

- Given：源文件存在但不在 git 仓库下
- When：请求 `display` 格式
- Then：剪贴板得到源文件的绝对路径（不是 buffer 名）

## US-0020：复制 region / 当前行的位置引用

作为用户，我希望把代码位置（可选含内容）作为引用粘贴给协作者或 AI agent。

### AC-0020-0010：region 位置

- Given：buffer 中存在有效 region
- When：请求仅复制位置（不含内容）
- Then：剪贴板得到形如 `@<path>#L<起始>-L<结束>` 的位置串

### AC-0020-0020：无 region 时复制当前行

- Given：buffer 中无 region
- When：请求复制位置
- Then：剪贴板得到当前行的 `@<path>#L<n>` 位置串

### AC-0020-0030：含内容模式

- Given：请求 with-content 变体
- When：调用
- Then：剪贴板内容包含位置串，以及用 Markdown 代码围栏包裹的选中/当前行原文

### AC-0020-0040：围栏长度自适应

- Given：选中内容含反引号序列
- When：生成围栏
- Then：围栏反引号数目至少比内容中最长反引号序列多 1，且不少于 3

### AC-0020-0050：保存后再复制

- Given：`cref-save-before-copy` 为 t 且 buffer 已修改且 visit 了文件
- When：复制 region 位置（含/不含内容均适用）
- Then：buffer 先被保存再生成位置串，消息以 `Saved. ` 开头

### AC-0020-0060：非 file-visiting buffer 不支持 region 引用

- Given：当前 buffer 不是 file-visiting buffer（包括 dired、临时 buffer 等）
- When：请求任何 region / 行位置复制（含/不含内容均适用）
- Then：操作以 `user-error` 终止，剪贴板与 `kill-ring` 均不被写入

> 说明：region 引用的语义是「某文件的某几行」。dired 列表行号、临时 buffer 内容都不构成可被其他读者打开的「文件 + 行号」坐标，直接复用路径解析会得到语义错误的 `@entry#L<list-line>`，因此显式拒绝。
