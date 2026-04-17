EMACS ?= emacs

.PHONY: test compile checkdoc clean

test: clean
	$(EMACS) --batch -l run-tests.el

compile:
	$(EMACS) --batch \
	  --eval '(setq agentmux-test-load-only t)' \
	  -l run-tests.el \
	  -f batch-byte-compile agentmux.el agentmux-test.el

checkdoc:
	$(EMACS) --batch \
	  --eval '(checkdoc-file "agentmux.el")'

clean:
	find . -maxdepth 1 -name '*.elc' -exec trash {} +
