# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概览

面向 Emacs 31+ 的个人配置。使用内置 `use-package` 搭配 MELPA/GNU ELPA/NonGNU ELPA 包源。灵感来自 purcell, redguardtoo 和 Centaur Emacs。

## 架构

### 启动流程

`early-init.el` -> `init.el` -> `lisp/init-package.el` -> 其余 `lisp/init-*.el` 按依赖顺序加载 -> `init-local.el`（可选覆盖）-> `custom.el`（仅在文件存在时加载）

### 模块组织（`lisp/`）

每个 `init-*.el` 是独立模块，通过 `require` 加载。`init.el` 中的加载顺序有依赖关系（例如 `init-general` 依赖 `init-evil`）。

模块分组：
- 核心：`init-package`, `init-custom`, `init-env`, `init-basic`（repeat-echo 内置）, `init-clipboard`
- 界面：`init-theme`（srcery）, `init-highlight`, `init-ui`（doom-modeline, dashboard, nerd-icons）
- Evil：`init-evil`（vim 键绑定 + evil-collection）, `init-keybind`（which-key，Emacs 30+ 内置）, `init-general`（SPC leader via general.el + transient 菜单）
- 补全：`init-search`（avy, ripgrep）, `init-completion`（vertico, consult, embark, company, orderless）, `init-yasnippet`
- 窗口/文件/会话：`init-window`, `init-dired`, `init-session`
- 编辑增强：`init-edit`（expreg, iedit, markdown-mode）
- 编程：`init-prog`（多语言 mode, flymake, breadcrumb）, `init-lsp`（eglot）, `init-go`, `init-python`
- 集成：`init-git`（magit, diff-hl）, `init-term`, `init-im`, `init-ai`（copilot, copilot-commit, agentmux）
- 工具：`init-utils`（`my/open-junk-file` 等自定义命令）
- 覆盖：`init-local`（机器级配置，最后加载，可选）

### 自定义包（`site-lisp/`）

- `copilot-commit` - 通过 Copilot LSP chat API 生成 conventional commit 消息。支持多语言（en/zh/ja）。快捷键：`C-c i`（插入）, `C-c I`（重新生成）
- `agentmux` - 通过 tmux 向 AI agent CLI（Claude Code 等）发送文件上下文。快捷键：`C-c a`
- `code-ref` - 以多种路径格式复制代码引用（绝对路径，git 相对路径，文件名，含/不含内容）。为 `init-general` 中的 transient 菜单提供支持
- `im-bridge` - 输入法切换与 Evil mode 集成

### 自定义变量系统

用户级变量定义在 `init-custom.el` 中，归属 `my` 自定义组，使用 `my/` 前缀。覆盖方式：
- `M-x customize-group RET my RET`
- 直接编辑 `custom.el`
- 在 `init-local.el` 中用 `setopt` 设置

## 测试

`copilot-commit`, `code-ref` 两个 site-lisp 包有独立的 `Makefile`，在对应包目录下运行：

```sh
cd site-lisp/copilot-commit && make test
cd site-lisp/code-ref && make test

# 字节编译和文档检查（在各包目录下运行）
cd site-lisp/copilot-commit && make compile
cd site-lisp/code-ref && make checkdoc
```

无顶层测试命令，各包独立测试。

## 包选型原则

- 优先使用 Emacs 内置功能，仅在内置无法满足或体验明显不足时引入第三方包
- 当前基于 Emacs 31，每次 Emacs 大版本更新后需根据新增内置功能重新筛查第三方包的必要性

## 代码约定

- 所有 `.el` 文件使用 `lexical-binding: t`
- 通过 `use-package` 配置包：外部包用 `:ensure t`，内置/site-lisp 包用 `:ensure nil`
- 全局默认延迟加载（`use-package-always-defer t`）
- 自定义函数和变量统一使用 `my/` 前缀
- 自定义变量用 `defcustom` 定义在 `init-custom.el` 中
- 项目级语言设置通过 `my/project-language` 变量（在 `.dir-locals.el` 中设置）

## Elisp 编辑须知

- 新增 `init-*.el` 模块时，必须在 `init.el` 中按正确位置添加 `require`
- `site-lisp/` 子目录会自动加入 `load-path`
- `init-local.el` 通过 `(require 'init-local nil t)` 加载，文件不存在时静默跳过
