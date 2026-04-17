# site-lisp 公共约束

`site-lisp/` 下的所有自定义包（`agentmux`、`code-ref`、`copilot-commit`、`im-bridge`、`vbnet-mode`）共同遵守以下约束。包级 SPEC / CLAUDE.md 与本文件冲突时按子目录优先。

## 失败语义：fail-fast，禁止 fallback

外部查询（tmux、`ps`、子进程、shell、LSP、网络、文件系统等）失败一律以 `user-error` 终止并指明原因。

禁止：

- `ignore-errors` 包裹会失败的调用
- `condition-case` 把错误压成 `nil` / 空字符串 / 默认值
- shell / make 用 `2>/dev/null`、`|| true`、`--ignore-errors` 等静默兜底
- 任何形式的「让上层不知道出错了」

`condition-case` 仅允许两种模式：

```elisp
;; 模式 A: 透传 user-error，并把其它 error 包装成带业务上下文的 user-error
(condition-case err
    BODY
  (user-error (signal (car err) (cdr err)))
  (error (user-error "Failed to <action>: %s" (error-message-string err))))

;; 模式 B: 仅包装为带业务上下文的 user-error（错误信息分层）
(condition-case err
    BODY
  (error (user-error "Failed to <action>: %s" (error-message-string err))))
```

唯一允许的「回退」是 SPEC 化的合法状态分支（如某个环境变量缺失走另一条明确路径），由显式条件区分，而非异常捕获。

错误类型映射沿用项目级 `CLAUDE.md`「错误处理规则」段。
