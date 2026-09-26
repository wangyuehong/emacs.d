SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-print-directory

EMACS ?= emacs
EMACS_BATCH := $(EMACS) -Q --batch --eval '(setq load-prefer-newer t)'
ELISP_FILES := tmpl-core.el tmpl.el
REPO_ROOT := $(abspath ../..)
# Host modes the tests open under -Q need these tree-sitter grammars.
GRAMMARS := html yaml go

.DEFAULT_GOAL := help

.PHONY: help
help: ## 列出所有可用目标
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'

.PHONY: all
all: compile test checkdoc trace integration ## 跑完整检查

.PHONY: compile
compile: ## 字节编译，有告警则失败
	$(EMACS_BATCH) -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(ELISP_FILES)

.PHONY: test
test: check-grammars ## 在 emacs -Q 下跑单元测试
	$(EMACS_BATCH) -l run-tests.el

.PHONY: checkdoc
checkdoc: ## 检查文档字符串，有告警则失败
	@for f in $(ELISP_FILES); do \
	  $(EMACS_BATCH) --eval "(let ((ok t)) (advice-add 'display-warning :before (lambda (&rest _) (setq ok nil))) (checkdoc-file \"$$f\") (kill-emacs (if ok 0 1)))" || exit 1; \
	done

.PHONY: trace
trace: ## 逐条列出 SPEC 的 AC 与覆盖它的测试（有 AC 无测试或编号写错则失败）
	@$(EMACS_BATCH) -l trace-ac.el

.PHONY: integration
integration: check-grammars ## 在 batch 下加载仓库 init.el 跑集成测试
	@# 真实 init 需要包目录与 tree-sitter 语法，因此不加 -Q
	cd $(REPO_ROOT) && $(EMACS) --batch -l site-lisp/tmpl/run-integration.el

.PHONY: clean
clean: ## 清理字节编译产物
	@# glob 在 recipe 的 shell 里展开，而不是 make 解析期：$(wildcard) 会漏掉
	@# 同一次调用内新产生的 .elc。
	@for f in *.elc; do \
	  [ ! -e "$$f" ] || trash "$$f"; \
	done

# 语法装在 ~/.emacs.d/tree-sitter/，不入库；缺失时停下并说明安装方法
.PHONY: check-grammars
check-grammars:
	@$(EMACS_BATCH) --eval "(let ((missing (seq-remove #'treesit-language-available-p '($(GRAMMARS))))) (when missing (message \"Missing tree-sitter grammars: %s. Install each with M-x treesit-install-language-grammar (sources in lisp/init-prog.el).\" (mapconcat #'symbol-name missing \" \")) (kill-emacs 1)))"
