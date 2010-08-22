# .zprofile -- loaded if login shell        .zshenv .zprofile .zshrc .zlogin
# .zshenv -- always loaded                  .zshenv .zprofile .zshrc .zlogin
# .zshrc -- loaded if interactive shell     .zshenv .zprofile .zshrc .zlogin

# Lines configured by zsh-newuser-install {{{
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt autocd notify inc_append_history autolist automenu share_history hist_ignore_dups
unsetopt beep equals

# }}}

# The following lines were added by compinstall {{{
zstyle :compinstall filename '/home/corrupt/.zshrc'

autoload -U compinit promptinit
compinit
promptinit; prompt walters

# }}}

# general {{{

#nice completion features including the fancy menu-mode
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' menu select=3
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Z}'
# End of lines added by compinstall

# completion:
#local myhosts
#myhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
#zstyle ':completion:*:(ssh|scp|sftp):*' hosts $_myhosts

# enable emacs mode
bindkey -e

setopt NO_BEEP

# }}}

# old school hotkeys {{{
bindkey '^[[5~' history-beginning-search-backward #pg-up
bindkey '^[[6~' history-beginning-search-forward #pg-dn

#screen
case "$TERM" in
	*screen*) #somehow screen translates keys differently
		bindkey '[1~' beginning-of-line
		bindkey '[4~' end-of-line
		bindkey '[3~' delete-char
		;;
	*)
		bindkey '^A' beginning-of-line #ctrl-a
		bindkey '^[OH' beginning-of-line #pos-1
		bindkey '^[[7~' beginning-of-line #pos-1-laptop
		bindkey '^[[H' beginning-of-line #pos-1 on mac keyboard?
		bindkey '^E' end-of-line #ctrl-e
		bindkey '^[OF' end-of-line #end
		bindkey '^[[8~' end-of-line #end-laptop
		bindkey '^[[F' end-of-line #end on mac keyboard?
		bindkey '^[[3~' delete-char #del
		bindkey '^J' backward-word #crtl-j
		bindkey '^K' forward-word #crtl-k
		;;
esac
# }}}

# aliases {{{

# general
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -la --color=auto'
alias lsd='ls --color=auto -d */'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias cls='clear'
alias hugs='hugs -Evim'
alias pmc="pacman-color"

#programs
alias stardict='stardict -h'

# global
alias -g G='|grep -i'
alias -g L='|less'

source .zsh-aliases

# }}}

# term-specific environment config {{{

# open (resume) a screen session if in a vt or on the server
case "$TERM" in 
	screen*)
		;;
	*screen)
		;;
	*screen*)
		#precmd()  { echo -ne "\e]0;%n@%m - %~\a" }
		;;
	*rxvt*)
		#screen
		#precmd()  { print -Pn "\e]0;%n@%m - %~\a" }
		;;
	*)
		#screen -d -R
		;;
esac

# }}}

# functions {{{

preexec () {
	case "$TERM" in 
		*screen*)
			echo -ne "\ek${1}\e\\"
		;;
		*rxvt*|*term*)
			print -Pn "\e]0;%n@%m - $1\a"
	esac
}

# }}}

# Prompt color defines {{{

BLACK="%{"$'\033[01;30m'"%}"
GREEN="%{"$'\033[01;32m'"%}"
RED="%{"$'\033[01;31m'"%}"
YELLOW="%{"$'\033[01;33m'"%}"
BLUE="%{"$'\033[01;34m'"%}"
BOLD="%{"$'\033[01;39m'"%}"
NORM="%{"$'\033[00m'"%}"

# }}}

# prompt config {{{

if [ "$USERNAME" = root ]; then
	export PS1="${RED}%m ${BLUE}~ %# ${NORM}" 
else
	export PS1="${GREEN}%n@%m ${BLUE}~ %# ${NORM}"
fi

# }}}
