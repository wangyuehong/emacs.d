# flymake-golangci-lint

A Flymake backend for golangci-lint version 2.x.

**Based on**: This project is based on and inspired by [flymake-golangci.el](https://gitlab.com/shackra/flymake-golangci) by Jorge Javier Araya Navarro, extensively rewritten to support modern golangci-lint v2 features.

## Features

- Full support for golangci-lint v2 command-line interface
- Configurable linters, timeout, and config file path
- Error parsing with line/column information
- Integrated with Emacs flymake for real-time syntax checking
- Manual linting mode with on-demand execution
- Auto-enable/disable control for flexible workflow

## Usage

This package should be placed in `site-lisp/flymake-golangci-lint/` and loaded automatically through Emacs' site-lisp mechanism.

**Configuration in init-prog.el:**
```elisp
(use-package flymake-easy)
```

**Configuration in init-go.el:**
```elisp
(use-package flymake-golangci-lint
  :ensure nil ;; site-lisp/flymake-golangci-lint
  :if (executable-find "golangci-lint")
  :after (go-mode flymake-easy)
  :hook (go-mode . flymake-golangci-lint-load))
```

## Configuration

```elisp
;; Custom configuration (optional)
(setq flymake-golangci-lint-config "/path/to/.golangci.yml")    ; Override auto-detection
(setq flymake-golangci-lint-timeout "10s")                     ; Shorter timeout for faster feedback
(setq flymake-golangci-lint-fast t)                            ; Enable fast mode for better performance
(setq flymake-golangci-lint-disable-linters '("typecheck"))    ; Disable slow linters

;; Control automatic enabling/disabling
(setq flymake-golangci-lint-auto-enable nil)                   ; Disable automatic linting
```

### Manual Linting

When automatic linting is disabled or for on-demand checking:

```elisp
;; Run golangci-lint manually on current buffer
M-x flymake-golangci-lint-run
```

**Note**: If `flymake-golangci-lint-config` is not set, golangci-lint will automatically search for configuration files (`.golangci.yml`, `.golangci.yaml`, etc.) starting from the current directory up to the project root.

## Performance Optimization

For faster real-time checking, consider these optimizations:

1. **Enable fast mode**: `(setq flymake-golangci-lint-fast t)`
2. **Reduce timeout**: `(setq flymake-golangci-lint-timeout "5s")`
3. **Disable slow linters**: `(setq flymake-golangci-lint-disable-linters '("typecheck" "staticcheck"))`
4. **Use project config** to limit linters:
   ```yaml
   # .golangci.yml
   run:
     timeout: 10s
   linters:
     fast: true
   ```

## Dependencies

- **flymake-easy**: Installed via MELPA, configured in `init-prog.el`
- **golangci-lint**: External CLI tool, must be available in PATH
- **Emacs 30.1+**: Required for modern flymake features

## Installation

1. Ensure golangci-lint is installed and available in PATH
2. Place this directory in `~/.emacs.d/site-lisp/flymake-golangci-lint/`
3. Add the configuration shown above to your init files