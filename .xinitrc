#!/bin/bash

# if no WM is set, default to xmonad
if [ -z $1 ]; then
	WM=xmonad
	#WM=startkde
else
	WM=$1
fi

if [ $WM == "xmonad" ]; then
	# stuff I only need with xmonad
	.bin/wprotate &
	#gnome-power-manager & #is started by gnome-session
	kdeinit4 &
	gnome-session &
	#wicd-client & #seems to be started by gnome-session
	klipper &
	urxvt &
fi

# stuff I always need
eval `/usr/bin/ssh-agent` > /var/tmp/ssh_agent &
xbindkeys &
xmodmap ~/.Xmodmap &
truecrypt /dev/sda7 /mnt/stuff --fs-options="umask=000" &

# exec with consolekit for hal
#exec ck-launch-session $WM
exec $WM
