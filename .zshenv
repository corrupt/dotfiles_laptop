# env variables {{{

#setting editor to anything but vi obviously sets edit mode to emacs
#which is more straightforward
#should be covered by 'bindkey e' anyway
export EDITOR=${EDITOR:-/usr/bin/vim}
export PAGER=${PAGER:-/bin/less}

# }}}
