SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-print-directory

EMACS ?= emacs
EMACS_BATCH := $(EMACS) --batch --eval '(setq load-prefer-newer t)'
ELISP_FILES := code-ref-core.el code-ref.el
ELC_FILES := $(ELISP_FILES:.el=.elc)

.DEFAULT_GOAL := help

.PHONY: help
help: ## 列出所有可用目标
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'

.PHONY: all
all: compile test checkdoc ## 跑完整检查

.PHONY: test
test: ## 跑测试
	$(EMACS_BATCH) \
	  -L . \
	  -l code-ref-test \
	  -f ert-run-tests-batch-and-exit

.PHONY: compile
compile: ## 字节编译
	$(EMACS_BATCH) -L . -f batch-byte-compile $(ELISP_FILES)

.PHONY: checkdoc
checkdoc: ## 检查文档字符串
	for f in $(ELISP_FILES); do \
	  $(EMACS_BATCH) --eval "(checkdoc-file \"$$f\")"; \
	done

.PHONY: clean
clean: ## 清理字节编译产物
	@for f in $(ELC_FILES); do \
	  [ ! -e "$$f" ] || trash "$$f"; \
	done
