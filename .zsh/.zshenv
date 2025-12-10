# -*- mode:shell-script -*-
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache
export XDG_STATE_HOME=$HOME/.local/state
export ZDOTDIR=$XDG_CONFIG_HOME/dotfiles/.zsh
export CLAUDE_CONFIG_DIR=$XDG_CONFIG_HOME/claude

# 1Password Environments から秘匿情報を読み込む
if [[ -p "$ZDOTDIR/.env.local" ]]; then
  # 名前付きパイプ（FIFO）から読み込む
  set -a
  source <(cat "$ZDOTDIR/.env.local")
  set +a
elif [[ -f "$ZDOTDIR/.env.local" ]]; then
  # 通常ファイルから読み込む
  set -a
  source "$ZDOTDIR/.env.local"
  set +a
fi

export ENHANCD_HYPHEN_NUM=50
