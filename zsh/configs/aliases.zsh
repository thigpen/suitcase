
###############################################################################

if [[ $OSTYPE = darwin* ]];
then
  alias my-cp-cwd='pwd | pbcopy'
  alias my-pbconvert="pbpaste | perl -pe 's/\r\n|\r/\n/g' | pbcopy"
  alias my-nice-top="/usr/bin/top -ocpu -R -F -s 2 -n30"

  alias my-wireless-stats='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I'

  alias my-osx-restart-audio='sudo pkill coreaudiod'
  alias my-osx-restart-wm='sudo killall -KILL Dock'
  alias my-osx-restart-bluetooth='sudo kextunload -b com.apple.iokit.BroadcomBluetoothHostControllerUSBTransport && sudo kextload -b com.apple.iokit.BroadcomBluetoothHostControllerUSBTransport'
  alias my-osx-restart-dns='sudo killall -HUP mDNSResponder && sudo killall mDNSResponderHelper && sudo dscacheutil -flushcache'

  alias my-osx-showfiles="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
  alias my-osx-hidefiles="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"
  
  alias my-osx-hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
  alias my-osx-showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
    
  # Set JAVA_HOME
  # alias osx_java_list="/usr/libexec/java_home -V"
  # alias osx_java_set_12="export JAVA_HOME=`/usr/libexec/java_home -v 12`"
  # alias osx_java_set_11="export JAVA_HOME=`/usr/libexec/java_home -v 11`"
  # alias osx_java_set_1_8="export JAVA_HOME=`/usr/libexec/java_home -v 1.8`"
fi

###############################################################################

if [[ $OSTYPE = aix* ]];
then
    # All gone from neu.edu
fi

###############################################################################

if [[ $OSTYPE = freebsd* ]];
then
    # All gone from techno.stanford.edu
fi

###############################################################################

if [[ $OSTYPE = linux* ]];
then
    # ...
fi

###############################################################################

if [[ -d $MY_SUITCASE_BREW_GNU_INSTALL ]];
then
    # Alias GNU versions 

    alias grep='grep --color'
    alias less='less -Xr'

    alias ls='$MY_SUITCASE_BREW_GNU_INSTALL/ls -F --color'

    alias my-rable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.R)"
    alias my-nrable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.^R)"
    alias my-wable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.W)"
    alias my-nwable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.^W)"
    alias my-xable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.X)"
    alias my-nxable="$MY_SUITCASE_BREW_GNU_INSTALL/ls -AFtrd *(.^X)"

    alias my-sort-ip='$MY_SUITCASE_BREW_GNU_INSTALL/sort -n -t . -k 1,1 -k 2,2 -k 3,3 -k 4,4'    

fi


alias mv='mv -i'
alias rm='rm -i'

alias dus='du -sckx * | sort -nr'
alias bkdir='cd $OLDPWD'
alias \?='history -fDd'
alias u=uptime
alias d='dirs -v'


alias my-git-log="git log -n 15 --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

alias my-nice-path='echo $PATH | tr ":" "\n" | sort'

alias my-rot13='tr A-MN-Za-mn-z N-ZA-Mn-za-m'
alias my-order-files-by-type='ls -l | /usr/bin/sort -n -r +4'

alias my-fc-stop='stty stop "^S"'    # Turn on flow control
alias my-fc-start='stty stop "^Q"'   # Turn on flow control
alias my-set-tty-bs='stty erase "^H"'
alias my-set-tty-del='stty erase "^?"'

alias my-show-256-color='for code ({000..255}) print -P -- "$code: %F{$code}This is how your text would look like%f"'

alias my-dw=/Applications/DeltaWalker/DeltaWalker.app/Contents/MacOS/DeltaWalker

###############################################################################
