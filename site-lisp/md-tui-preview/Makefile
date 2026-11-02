SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-print-directory

EMACS ?= emacs
EMACS_BATCH := $(EMACS) --batch -L ../../lisp --eval '(setq load-prefer-newer t)'
ELISP_FILES := terminal-browser-embed.el md-tui-preview.el

.DEFAULT_GOAL := help

.PHONY: help
help: ## 列出所有可用目标
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'

.PHONY: all
all: compile test checkdoc trace ## 跑完整检查

.PHONY: test
test: ## 跑测试
	$(EMACS_BATCH) -L . -l run-tests.el

.PHONY: compile
compile: ## 字节编译
	$(EMACS_BATCH) -L . -f batch-byte-compile $(ELISP_FILES)

.PHONY: checkdoc
checkdoc: ## 检查文档字符串，有告警则失败
	@for f in $(ELISP_FILES); do \
	  $(EMACS_BATCH) --eval "(let ((ok t)) (advice-add 'display-warning :before (lambda (&rest _) (setq ok nil))) (checkdoc-file \"$$f\") (kill-emacs (if ok 0 1)))" || exit 1; \
	done

.PHONY: trace
trace: ## 逐条列出 SPEC 的 AC 与覆盖它的测试（AC 编号写错则失败）
	@$(EMACS_BATCH) -l trace-ac.el

.PHONY: clean
clean: ## 清理字节编译产物
	@# glob 在 recipe 的 shell 里展开，而不是 make 解析期：$(wildcard) 会漏掉
	@# 同一次调用内新产生的 .elc（`make compile clean` 就什么都清不掉）。
	@for f in *.elc; do \
	  [ ! -e "$$f" ] || trash "$$f"; \
	done
