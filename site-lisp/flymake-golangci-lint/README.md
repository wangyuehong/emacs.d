# flymake-golangci-lint

A modern Flymake backend for golangci-lint version 2.x and Emacs 30+.

Based on: This project is based on [flymake-golangci.el](https://gitlab.com/shackra/flymake-golangci) by Jorge Javier Araya Navarro, rewritten for Emacs 30+ compatibility.

## Features

- Native flymake integration - No external dependencies, uses modern Emacs 30+ flymake API
- No flymake-easy dependency - Uses native flymake API
- Async processing - Non-blocking operation
- golangci-lint v2+ compatibility - Updated command interface
- Error parsing - Pattern matching with rx macro
- Configuration options - Control over linters, timeouts, and behavior
- Fast mode - Selective linter control
- Manual and automatic modes - Flexible usage

## Installation

### Dependencies

- Emacs 30.1+ - Required for modern flymake API
- golangci-lint - External CLI tool, must be available in PATH
- No additional Emacs packages - Uses native flymake, no flymake-easy dependency

### Setup

1. Ensure golangci-lint is installed and available in PATH
2. Place this directory in `~/.emacs.d/site-lisp/flymake-golangci-lint/`
3. Add configuration to your init files

## Usage

Configuration in init-go.el:
```elisp
(use-package flymake-golangci-lint
  :ensure nil ;; site-lisp/flymake-golangci-lint
  :if (executable-find "golangci-lint")
  :after go-mode
  :hook (go-mode . flymake-golangci-lint-load))
```

Simplified configuration (if using site-lisp autoload):
```elisp
;; Automatically loads when golangci-lint is available
(add-hook 'go-mode-hook 'flymake-golangci-lint-load)
```

## Configuration

### Basic Configuration Options

```elisp
;; Performance optimization
(setq flymake-golangci-lint-fast t)                            ; Enable fast mode
(setq flymake-golangci-lint-timeout "10s")                     ; Set timeout for faster feedback

;; Linter control
(setq flymake-golangci-lint-disable-linters '("typecheck"))    ; Disable slow/specific linters
(setq flymake-golangci-lint-enable-linters '("gofmt" "goimports")) ; Enable only specific linters

;; Config file (optional - auto-detected by default)
(setq flymake-golangci-lint-config "/path/to/.golangci.yml")   ; Override auto-detection

;; Control automatic enabling
(setq flymake-golangci-lint-auto-enable nil)                  ; Disable automatic linting
(setq flymake-golangci-lint-tests nil)                        ; Skip test files analysis
```

### Complete Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `flymake-golangci-lint-executable` | `"golangci-lint"` | Executable name or path |
| `flymake-golangci-lint-config` | `nil` | Config file path (auto-detected if nil) |
| `flymake-golangci-lint-timeout` | `"1m"` | Timeout for golangci-lint execution |
| `flymake-golangci-lint-enable-linters` | `nil` | List of linters to enable |
| `flymake-golangci-lint-disable-linters` | `nil` | List of linters to disable |
| `flymake-golangci-lint-tests` | `t` | Whether to analyze test files |
| `flymake-golangci-lint-fast` | `nil` | Run only fast linters |
| `flymake-golangci-lint-auto-enable` | `t` | Auto-enable in go-mode |

### Manual Usage

When automatic linting is disabled or for on-demand checking:

```elisp
;; Run golangci-lint manually on current buffer
M-x flymake-golangci-lint-run
```

This command will:
- Enable flymake-mode if not already active
- Add the golangci-lint backend to flymake
- Start linting the current buffer
- Display results in flymake interface

Note: Config file auto-detection searches for `.golangci.yml`, `.golangci.yaml`, etc. from current directory up to project root.

## Performance Optimization

The backend uses async processing. You can optimize further:

### Recommended Settings

```elisp
;; Fast, responsive configuration
(setq flymake-golangci-lint-fast t)                            ; Use only fast linters
(setq flymake-golangci-lint-timeout "5s")                      ; Shorter timeout
(setq flymake-golangci-lint-disable-linters '("typecheck" "staticcheck")) ; Skip slow linters
```

### Project-level Configuration

Create `.golangci.yml` in your project root:

```yaml
# .golangci.yml - Optimized for real-time editing
run:
  timeout: 10s
  
linters:
  fast: true
  disable:
    - typecheck      # Slow, IDE already provides this
    - staticcheck    # Comprehensive but slow
    
linters-settings:
  # Configure specific linters if needed
```

### Performance Tips

1. Use fast mode - Enables only fast linters for real-time feedback
2. Selective linter control - Disable comprehensive but slow analysis tools
3. Shorter timeouts - Faster feedback, cancel slow operations
4. Project-specific config - Tailor linting to project needs
5. Async processing - Callback-based implementation prevents blocking