# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

日本語で回答してください

## Repository Overview

This is a dotfiles repository for development environments, primarily focused on shell configuration (Zsh) and development tools. The repository contains personal configuration files for a multi-language development setup.

> **Note**: Emacs 設定は [nanasess/home-manager](https://github.com/nanasess/home-manager) の `modules/emacs/` に移行済みです。

## Installation and Setup

The repository uses a simple symbolic linking approach:
```bash
sh ./install
```

This creates symlinks in the home directory for:
- `.zsh` → Zsh shell configuration
- `.zshenv` → Zsh environment variables
- `.signature` → Email signature
- `phpactor` → PHP language server configuration

## Architecture

### Configuration Structure
- **Root level**: Core dotfiles (`.bashrc`, `.signature`, etc.)
- **`.zsh/`**: Complete Zsh configuration with PowerLevel10k theme, aliases, and environment setup
- **`phpactor/`**: PHP language server configuration
- **`sheldon/`**: Shell plugin manager configuration

### Package Managers
- **Sheldon**: Zsh plugin management

## Key Files
- `install`: Main installation script
- `.zsh/.zshrc`: Primary Zsh configuration
- `phpactor/phpactor.yml`: PHP language server settings
- `sheldon/plugins.toml`: Shell plugin definitions
