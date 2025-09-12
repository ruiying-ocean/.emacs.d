# Emacs Configuration Dependency Management
# Manages external dependencies for enhanced Emacs functionality

.PHONY: help install install-brew install-npm install-python install-lsp install-submodules check-deps clean

# Default target
help:
	@echo "Emacs Configuration Dependency Manager"
	@echo ""
	@echo "Available targets:"
	@echo "  install      - Install all dependencies"
	@echo "  install-brew - Install Homebrew dependencies (macOS)"
	@echo "  install-npm  - Install Node.js dependencies"
	@echo "  install-python - Install Python language server"
	@echo "  install-lsp  - Install LSP Bridge dependencies"
	@echo "  install-submodules - Initialize and update git submodules"
	@echo "  check-deps   - Check which dependencies are installed"
	@echo "  clean        - Clean package caches"
	@echo "  help         - Show this help message"

# Install all dependencies
install: install-brew install-npm install-python install-lsp install-submodules
	@echo "All dependencies installed successfully!"

# Install Homebrew dependencies (macOS only)
install-brew:
	@echo "Installing Homebrew dependencies..."
	@if command -v brew >/dev/null 2>&1; then \
		brew install pngpaste poppler mupdf gcc make; \
		if ! brew list emacs-mac >/dev/null 2>&1; then \
			echo "Consider installing emacs-mac for enhanced features:"; \
			echo "  brew tap railwaycat/emacsmacport"; \
			echo "  brew install emacs-mac"; \
		fi; \
	else \
		echo "Homebrew not found. Please install Homebrew first:"; \
		echo "  /bin/bash -c \"\$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""; \
		exit 1; \
	fi

# Install Node.js dependencies
install-npm:
	@echo "Installing Node.js dependencies..."
	@if command -v npm >/dev/null 2>&1; then \
		npm install -g livedown; \
	else \
		echo "npm not found. Please install Node.js first:"; \
		echo "  brew install node"; \
		exit 1; \
	fi

# Install Python language server
install-python:
	@echo "Installing Python language server..."
	@if command -v npm >/dev/null 2>&1; then \
		npm install -g pyright; \
	else \
		echo "npm not found. Please install Node.js first"; \
		exit 1; \
	fi

# Install LSP Bridge dependencies
install-lsp:
	@echo "Installing LSP Bridge dependencies..."
	@if command -v python3 >/dev/null 2>&1; then \
		python3 -m pip install epc orjson sexpdata six setuptools paramiko rapidfuzz; \
	else \
		echo "Python3 not found. Please install Python3 first:"; \
		echo "  brew install python3"; \
		exit 1; \
	fi
	@echo "Note: Language servers are installed automatically by lsp-bridge"

# Initialize and update git submodules
install-submodules:
	@echo "Initializing and updating git submodules..."
	@git submodule update --init --recursive
	@if [ -f lsp-bridge/python-lsp-bridge ] && command -v uv >/dev/null 2>&1; then \
		echo "Creating symlink for python-lsp-bridge wrapper..."; \
		if command -v brew >/dev/null 2>&1; then \
			ln -sf $(PWD)/lsp-bridge/python-lsp-bridge $(shell brew --prefix)/bin/python-lsp-bridge; \
		else \
			echo "Homebrew not found. Please manually add lsp-bridge/python-lsp-bridge to your PATH"; \
		fi; \
	fi
	@echo "Git submodules updated successfully!"

# Check which dependencies are installed
check-deps:
	@echo "Checking dependency status..."
	@echo ""
	@echo "System Dependencies:"
	@command -v pngpaste >/dev/null 2>&1 && echo "  ✓ pngpaste" || echo "  ✗ pngpaste (run: brew install pngpaste)"
	@command -v pdftoppm >/dev/null 2>&1 && echo "  ✓ poppler" || echo "  ✗ poppler (run: brew install poppler)"
	@command -v mutool >/dev/null 2>&1 && echo "  ✓ mupdf" || echo "  ✗ mupdf (run: brew install mupdf)"
	@command -v gcc >/dev/null 2>&1 && echo "  ✓ gcc" || echo "  ✗ gcc (run: brew install gcc)"
	@command -v make >/dev/null 2>&1 && echo "  ✓ make" || echo "  ✗ make (run: brew install make)"
	@brew list emacs-mac >/dev/null 2>&1 && echo "  ✓ emacs-mac" || echo "  ○ emacs-mac (optional: brew install emacs-mac)"
	@echo ""
	@echo "Node.js Dependencies:"
	@command -v livedown >/dev/null 2>&1 && echo "  ✓ livedown" || echo "  ✗ livedown (run: npm install -g livedown)"
	@command -v pyright >/dev/null 2>&1 && echo "  ✓ pyright" || echo "  ✗ pyright (run: npm install -g pyright)"
	@echo ""
	@echo "LSP Bridge Dependencies:"
	@python3 -c "import epc, orjson, sexpdata, six, paramiko, rapidfuzz" 2>/dev/null && echo "  ✓ Python packages" || echo "  ✗ Python packages (run: make install-lsp)"
	@command -v python3 >/dev/null 2>&1 && echo "  ✓ Python3" || echo "  ✗ Python3 (run: brew install python3)"
	@echo ""
	@echo "Package Managers:"
	@command -v brew >/dev/null 2>&1 && echo "  ✓ Homebrew" || echo "  ✗ Homebrew (required for macOS dependencies)"
	@command -v npm >/dev/null 2>&1 && echo "  ✓ npm" || echo "  ✗ npm (required for Node.js dependencies)"

# Clean package caches
clean:
	@echo "Cleaning package caches..."
	@if command -v brew >/dev/null 2>&1; then \
		brew cleanup; \
	fi
	@if command -v npm >/dev/null 2>&1; then \
		npm cache clean --force; \
	fi
	@echo "Package caches cleaned!"

# Install minimal dependencies (core functionality only)
install-minimal: install-npm
	@echo "Installing minimal dependencies for core functionality..."

# Development target for testing Makefile
test:
	@echo "Testing Makefile functionality..."
	@make check-deps