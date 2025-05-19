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

# see https://qiita.com/mfunaki/items/db6e1ffcf1e6f1eff252#wsl2%E5%81%B4%E3%81%A7%E3%81%AE%E8%A8%AD%E5%AE%9A%E5%8B%95%E4%BD%9C%E7%A2%BA%E8%AA%8D
# need `ps -ww` to get non-truncated command for matching
# use square brackets to generate a regex match for the process we want but that doesn't match the grep command running it!
ALREADY_RUNNING=$(ps -auxww | grep -q "[n]piperelay.exe -ei -s //./pipe/openssh-ssh-agent"; echo $?)
if [[ $ALREADY_RUNNING != "0" ]]; then
    if [[ -S $SSH_AUTH_SOCK ]]; then
        # not expecting the socket to exist as the forwarding command isn't running (http://www.tldp.org/LDP/abs/html/fto.html)
        echo "removing previous socket..."
        rm $SSH_AUTH_SOCK
    fi
    echo "Starting SSH-Agent relay..."
    # setsid to force new session to keep running
    # set socat to listen on $SSH_AUTH_SOCK and forward to npiperelay which then forwards to openssh-ssh-agent on windows
    (setsid socat UNIX-LISTEN:$SSH_AUTH_SOCK,fork EXEC:"npiperelay.exe -ei -s //./pipe/openssh-ssh-agent",nofork &) >/dev/null 2>&1
fi

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
