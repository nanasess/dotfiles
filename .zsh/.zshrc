# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/dotfiles/.zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# -*- mode:shell-script -*-
#
# dot.zshrc
#
[ -f $ZDOTDIR/.zaliases ] && source $ZDOTDIR/.zaliases
[ -f $ZDOTDIR/.zcompctl ] && source $ZDOTDIR/.zcompctl
if which sheldon > /dev/null
then
    eval "$(sheldon --config-dir $ZDOTDIR/../sheldon source)"
fi

# Emacs style key binding
bindkey -e

bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

# colors enabled
autoload -U colors
colors

setopt hist_ignore_space
setopt auto_list
setopt auto_pushd
setopt pushd_ignore_dups
setopt extended_glob
setopt hist_expand
setopt printeightbit
setopt correct

cdpath=($HOME)
setopt extended_history
setopt inc_append_history
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify

HISTFILE=$ZDOTDIR/.zsh-history
HISTSIZE=10000
SAVEHIST=10000000

export LANG=ja_JP.UTF-8

export PAGER=less
export LESSCHARSET=utf-8
export LESS="-SXFR"
export RSYNC_RSH=ssh
export JQ_COLORS="0;33:0;33:0;33:0;35:0;36:0;31:0;31"
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=~/.1password/agent.sock

# 1Password Shell Plugins
[[ -f ~/.config/op/plugins.sh ]] && source ~/.config/op/plugins.sh

if [ ! -n "${TERM}" ]; then
    TERM=xterm-256color
fi

PROMPT="%{[31m%}%~%%%{[m%} "
PROMPT2="%{[31m%}%_%%%{[m%} "
SPROMPT="%{[31m%}%r is correct? [n,y,a,e]:%{[m%} "
if [ -n "${REMOTEHOST}${SSH_CONNECTION}" ]; then
    PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
fi

[ -f $ZDOTDIR/.zshrc.mine ] && source $ZDOTDIR/.zshrc.mine
[ -f $ZDOTDIR/eterm.zsh ] && source $ZDOTDIR/eterm.zsh

eval "$(fzf --zsh)"

if which onedrive > /dev/null
then
    # onedrive --synchronize --single-directory 'emacs' > /dev/null &
fi
# if [[ ! -n $TMUX ]]; then
#     tmux new-session
# fi

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

# To customize prompt, run `p10k configure` or edit ~/dotfiles/.zsh/.p10k.zsh.
[[ ! -f ${XDG_CONFIG_HOME}/dotfiles/.zsh/.p10k.zsh ]] || source ${XDG_CONFIG_HOME}/dotfiles/.zsh/.p10k.zsh

. "$HOME/.local/share/../bin/env"

# bun completions
[ -s "/home/nanasess/.bun/_bun" ] && source "/home/nanasess/.bun/_bun"

