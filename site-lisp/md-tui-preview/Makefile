SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-print-directory

EMACS ?= emacs
EMACS_BATCH := $(EMACS) --batch --eval '(setq load-prefer-newer t)'
ELISP_FILES := md-tui-preview-core.el md-tui-preview.el
ELC_FILES := $(ELISP_FILES:.el=.elc)

# 自带 glow 风格文件由 glamour 上游 "dark" 风格叠加布局覆盖生成。
# GLAMOUR_REF pin 住上游版本（对应 glow 2.1.2 所依赖的 glamour），
# 避免上游改动无声漂移；glamour 采用 MIT 许可。
GLAMOUR_REF := v0.10.0
GLAMOUR_STYLE_URL := https://raw.githubusercontent.com/charmbracelet/glamour/$(GLAMOUR_REF)/styles/dark.json
STYLE_JSON := md-tui-preview-style.json
STYLE_JQ := md-tui-preview-style.jq
CURL ?= curl
JQ ?= jq

.DEFAULT_GOAL := help

.PHONY: help
help: ## 列出所有可用目标
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'

.PHONY: all
all: compile test checkdoc ## 跑完整检查

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

.PHONY: style
style: ## 从 pin 住的 glamour 上游风格重新生成自带风格文件
	@# 先写临时文件再 mv：curl/jq 失败时不截断已提交的运行时 $(STYLE_JSON)
	$(CURL) -fsSL "$(GLAMOUR_STYLE_URL)" | $(JQ) --indent 2 -f $(STYLE_JQ) > $(STYLE_JSON).tmp
	mv -f $(STYLE_JSON).tmp $(STYLE_JSON)

.PHONY: style-check
style-check: ## 校验自带风格与「上游 + 覆盖」一致（防 drift；需联网，不进 all）
	@diff -u $(STYLE_JSON) <($(CURL) -fsSL "$(GLAMOUR_STYLE_URL)" | $(JQ) --indent 2 -f $(STYLE_JQ))
	@echo "style-check: $(STYLE_JSON) 与 glamour $(GLAMOUR_REF) + $(STYLE_JQ) 一致"

.PHONY: clean
clean: ## 清理字节编译产物
	@for f in $(ELC_FILES); do \
	  [ ! -e "$$f" ] || trash "$$f"; \
	done
