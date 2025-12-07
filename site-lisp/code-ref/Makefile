# Makefile for code-ref

EMACS ?= emacs
ELISP_FILES = code-ref-core.el code-ref.el
TEST_FILES = code-ref-test.el

.PHONY: all test compile clean help

all: compile test

# Run all tests
test:
	@echo "Running code-ref test suite..."
	@$(EMACS) --batch \
		-L . \
		-l code-ref-test \
		-f ert-run-tests-batch-and-exit

# Compile elisp files
compile:
	@echo "Compiling elisp files..."
	@$(EMACS) --batch -L . -f batch-byte-compile $(ELISP_FILES)

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@rm -f *.elc

# Check documentation
checkdoc:
	@echo "Checking documentation..."
	@for f in $(ELISP_FILES); do \
		$(EMACS) --batch --eval "(checkdoc-file \"$$f\")"; \
	done

help:
	@echo "Available targets:"
	@echo "  all      - Compile and test"
	@echo "  test     - Run all tests"
	@echo "  compile  - Compile elisp files"
	@echo "  clean    - Remove compiled files"
	@echo "  checkdoc - Check documentation"
	@echo "  help     - Show this help message"
