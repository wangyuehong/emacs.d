EMACS ?= emacs

.PHONY: test compile checkdoc clean

test:
	$(EMACS) --batch -l run-tests.el

compile:
	$(EMACS) --batch \
	  -l run-tests.el \
	  --eval '(byte-compile-file "copilot-commit-core.el")' \
	  --eval '(byte-compile-file "copilot-commit.el")'

checkdoc:
	$(EMACS) --batch \
	  --eval '(checkdoc-file "copilot-commit-core.el")' \
	  --eval '(checkdoc-file "copilot-commit.el")'

clean:
	rm -f *.elc
