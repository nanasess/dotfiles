# -*- mode:shell-script -*-
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache
export XDG_STATE_HOME=$HOME/.local/state
export ZDOTDIR=$XDG_CONFIG_HOME/dotfiles/.zsh
export CLAUDE_CONFIG_DIR=$XDG_CONFIG_HOME/claude

# 1Password Environments から秘匿情報を読み込む（タイムアウト付き）
if [[ -p "$ZDOTDIR/.env.local" ]]; then
  # 名前付きパイプ（FIFO）から読み込む（1秒でタイムアウト）
  _OP_ENV_CONTENT=$(timeout 1 cat "$ZDOTDIR/.env.local" 2>/dev/null)
  if [[ -n "$_OP_ENV_CONTENT" ]]; then
    set -a
    source <(printf '%s\n' "$_OP_ENV_CONTENT")
    set +a
  else
    echo "\e[33m[WARNING] Could not load secrets: 1Password is not running.\e[0m" >&2
    echo "\e[33m          Please start 1Password and open a new shell.\e[0m" >&2
  fi
  unset _OP_ENV_CONTENT
elif [[ -f "$ZDOTDIR/.env.local" ]]; then
  # 通常ファイルから読み込む
  set -a
  source "$ZDOTDIR/.env.local"
  set +a
fi

export ENHANCD_HYPHEN_NUM=50
