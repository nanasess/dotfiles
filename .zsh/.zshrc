# -*- mode:shell-script -*-
#
# dot.zshrc
#
[ -f $ZDOTDIR/.zaliases ] && source $ZDOTDIR/.zaliases
[ -f $ZDOTDIR/.zcompctl ] && source $ZDOTDIR/.zcompctl

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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if which onedrive > /dev/null
then
    # onedrive --synchronize --single-directory 'emacs' > /dev/null &
fi
# if [[ ! -n $TMUX ]]; then
#     tmux new-session
# fi
