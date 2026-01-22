# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

日本語で回答してください

## Repository Overview

This is a comprehensive dotfiles repository for development environments, primarily focused on Emacs, shell configuration (Zsh), and development tools. The repository contains personal configuration files for a multi-language development setup including PHP, JavaScript/TypeScript, Ruby, and Python.

## Installation and Setup

The repository uses a simple symbolic linking approach:
```bash
sh ./install
```

This creates symlinks in the home directory for:
- `.emacs.d` → Emacs configuration
- `.zsh` → Zsh shell configuration
- `.zshenv` → Zsh environment variables
- `.signature` → Email signature
- `phpactor` → PHP language server configuration

## Development Commands

### Emacs Configuration
- **Compile Emacs Lisp**: `cd ~/.emacs.d && find . -name '*.el' | xargs emacs -Q -batch -l ~/.emacs.d/init.el -f batch-byte-compile`
- **Clean compiled files**: `cd ~/.emacs.d && find . -name '*.elc' -delete -print`
- **Test Emacs configuration**: `emacs -Q -l .emacs.d/early-init.el -l .emacs.d/init.el --batch`

### Package Management
- **Install PHP dependencies**: `composer install` (in `.emacs.d/bin/`)
- **Install Node.js dependencies**: `yarn install --frozen-lockfile` (in `.emacs.d/bin/`)
- **Install Ruby dependencies**: `bundle install` (in `.emacs.d/bin/`)

### CI/CD Commands (from .github/workflows/ci.yml)
The CI pipeline tests the configuration with:
- Python 3.12, Ruby 3.2, PHP 8.4, Node.js 22
- Emacs release-snapshot version
- All package installations and Emacs batch loading

## Architecture

### Configuration Structure
- **Root level**: Core dotfiles (`.bashrc`, `.signature`, etc.)
- **`.zsh/`**: Complete Zsh configuration with PowerLevel10k theme, aliases, and environment setup
- **`.emacs.d/`**: Comprehensive Emacs configuration with package management via el-get
- **`phpactor/`**: PHP language server configuration
- **`sheldon/`**: Shell plugin manager configuration

### Development Tool Integration
The repository includes language servers and development tools for:
- **PHP**: PHPStan, PHP-CS-Fixer, Phpactor, PsySH
- **JavaScript/TypeScript**: TypeScript language server, various VS Code language servers
- **Ruby**: Solargraph language server
- **Bash**: Bash language server
- **Docker**: Dockerfile language server
- **YAML**: YAML language server
- **Markdown**: Mermaid CLI for diagrams

### Package Management
- **Eask**: Emacs package management (configured in root `Eask` file)
- **el-get**: Emacs package manager (lock file tracked)
- **Composer**: PHP dependencies
- **Yarn**: JavaScript dependencies
- **Bundler**: Ruby dependencies
- **Sheldon**: Zsh plugin management

## Key Files
- `install`: Main installation script
- `Eask`: Emacs package configuration requiring Emacs 29.3+
- `.emacs.d/init.el`: Main Emacs configuration entry point
- `.zsh/.zshrc`: Primary Zsh configuration
- `phpactor/phpactor.yml`: PHP language server settings
- `sheldon/plugins.toml`: Shell plugin definitions
