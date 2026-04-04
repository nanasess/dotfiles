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
- **Test Emacs configuration**: `emacs --init-directory .emacs.d -l .emacs.d/early-init.el -l .emacs.d/init.el --batch`
- **Generate lock file**: `ELPACA_WRITE_LOCK=1 emacs --init-directory .emacs.d -l .emacs.d/early-init.el -l .emacs.d/init.el --batch`
- **Clean compiled files**: `cd ~/.emacs.d && find . -name '*.elc' -delete -print`

> **Note**: `--batch` は `-q` を含意するため init.el を自動ロードしません。`--init-directory` で `user-emacs-directory` を設定しつつ、`-l` で明示的にロードする必要があります。

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
- **`.emacs.d/`**: Comprehensive Emacs configuration with package management via elpaca + use-package
- **`phpactor/`**: PHP language server configuration
- **`sheldon/`**: Shell plugin manager configuration

### Emacs Package Management
- **elpaca**: Async package manager with use-package integration
  - Bootstrap code in `init.el` (elpaca installer v0.12)
  - Lock file: `.emacs.d/elpaca.lock` — version pinning via `elpaca-lock-file`
  - GUI からロックファイル更新: `M-x elpaca-write-lock-file`
  - Batch からロックファイル生成: `ELPACA_WRITE_LOCK=1` 環境変数を設定して batch 実行
- **use-package**: Emacs 30 組み込みの宣言的パッケージ設定マクロ
  - `:ensure` で elpaca 経由のインストール
  - `:ensure nil` で組み込みパッケージの設定
  - GitHub リポジトリは `:ensure (:host github :repo "owner/repo")` で指定

### Development Tool Integration
The repository includes language servers and development tools for:
- **PHP**: PHPStan, PHP-CS-Fixer, Phpactor, PsySH
- **JavaScript/TypeScript**: TypeScript language server, various VS Code language servers
- **Ruby**: Solargraph language server
- **Bash**: Bash language server
- **Docker**: Dockerfile language server
- **YAML**: YAML language server
- **Markdown**: Mermaid CLI for diagrams

### Other Package Managers
- **Composer**: PHP dependencies
- **Yarn**: JavaScript dependencies
- **Bundler**: Ruby dependencies
- **Sheldon**: Zsh plugin management

## Key Files
- `install`: Main installation script
- `.emacs.d/early-init.el`: Early Emacs initialization (GC, native-comp, package-enable-at-startup)
- `.emacs.d/init.el`: Main Emacs configuration entry point (elpaca bootstrap, use-package declarations)
- `.emacs.d/elpaca.lock`: Package version lock file (replaces el-get.lock)
- `.zsh/.zshrc`: Primary Zsh configuration
- `phpactor/phpactor.yml`: PHP language server settings
- `sheldon/plugins.toml`: Shell plugin definitions
