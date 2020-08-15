###############################################################################
#
# Zstyle ...
#
###############################################################################

# Report settings
# > zstyle -L 

# See https://gist.github.com/X4/6346787

# Configure caching ...
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

# Colorization ... 	
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*'	       list-colors  'reply=( "=(#b)(*$PREFIX)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle -e ':completion:*:-command-:*:commands'	list-colors 'reply=( '\''=(#b)('\''$words[CURRENT]'\''|)*-- #(*)=0=38;5;45=38;5;136'\'' '\''=(#b)('\''$words[CURRENT]'\''|)*=0=38;5;45'\'' )'

#------------------------------------------------------------------------------

# Prepare a defined hosts list and set ...
#zstyle -e ':completion:*' hosts 'reply=($myhosts)'

###############################################################################
