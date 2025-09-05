# Makefile for flymake-golangci-lint

EMACS ?= emacs
ELISP_FILES = flymake-golangci-lint.el
TEST_FILES = test-flymake-golangci-lint-clean.el

.PHONY: all test compile clean help coverage

all: compile test

# Run all tests
test:
	@echo "Running flymake-golangci-lint test suite..."
	@$(EMACS) --batch -l run-tests.el

# Compile elisp files
compile:
	@echo "Compiling elisp files..."
	@$(EMACS) --batch -f batch-byte-compile $(ELISP_FILES)

# Run specific user story tests
test-us%:
	@echo "Running tests for User Story $*..."
	@$(EMACS) --batch -l $(ELISP_FILES) -l $(TEST_FILES) \
		--eval "(ert-run-tests-batch \"test-.*us$*.*\")"

# Run tests with coverage (requires undercover package)
coverage:
	@echo "Running tests with coverage..."
	@$(EMACS) --batch \
		--eval "(require 'undercover)" \
		--eval "(undercover \"*.el\")" \
		-l $(ELISP_FILES) -l $(TEST_FILES) \
		--eval "(ert-run-tests-batch t)"

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@rm -f *.elc

# Interactive test runner
test-interactive:
	@echo "Starting interactive test session..."
	@$(EMACS) -l $(ELISP_FILES) -l $(TEST_FILES) \
		--eval "(test-flymake-golangci-lint-acceptance-criteria-summary)"

# Lint the code
lint:
	@echo "Linting elisp files..."
	@$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'elisp-lint) (package-refresh-contents) (package-install 'elisp-lint))" \
		--eval "(require 'elisp-lint)" \
		--eval "(elisp-lint-files-batch '(\"$(ELISP_FILES)\"))"

# Check documentation
checkdoc:
	@echo "Checking documentation..."
	@$(EMACS) --batch --eval "(checkdoc-file \"$(ELISP_FILES)\")"

# Install for development
install-dev:
	@echo "Installing development dependencies..."
	@$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'undercover)" \
		--eval "(package-install 'elisp-lint)"

help:
	@echo "Available targets:"
	@echo "  all           - Compile and test"
	@echo "  test          - Run all tests"
	@echo "  test-us<N>    - Run tests for User Story N (e.g., test-us1)"
	@echo "  compile       - Compile elisp files"
	@echo "  coverage      - Run tests with coverage"
	@echo "  clean         - Remove compiled files"
	@echo "  test-interactive - Start interactive test session"
	@echo "  lint          - Lint the code"
	@echo "  checkdoc      - Check documentation"
	@echo "  install-dev   - Install development dependencies"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make test           # Run all tests"
	@echo "  make test-us1       # Run User Story 1 tests"
	@echo "  make coverage       # Run with coverage report"