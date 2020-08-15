###############################################################################
#
# Zsh config ...
#
# Depends on Powerline 10k
#
###############################################################################

# Where did you git clone as ...
MY_SUITCASE_ZSH_HOME=${HOME}/suitcase/zsh

MY_SUITCASE_ZSH_PLUGINS=${MY_SUITCASE_ZSH_HOME}/plugins/scm/github

# GNU utilities (brew install coreutils) ...
MY_SUITCASE_BREW_GNU_INSTALL=/usr/local/opt/coreutils/libexec/gnubin

###############################################################################
#
# Completions ...
#
###############################################################################

# Import completion files which are denoted by a "_" prefix ...
# Import by adding directory to FPATH before the compinit command

# From HomeBrew ...
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh/site-functions/:$FPATH
    FPATH=$(brew --prefix)/share/zsh-completions/:$FPATH
fi

# From docker ...
if type docker &>/dev/null; then
    FPATH=${MY_SUITCASE_ZSH_PLUGINS}/docker/:$FPATH
fi

# For AWS Assume Role ...
[[ -d ${MY_SUITCASE_ZSH_PLUGINS}/assume-role ]] || FPATH=${MY_SUITCASE_ZSH_PLUGINS}/assume-role:$FPATH

rm -f ~/.zcompdump
autoload -Uz compinit
compinit


###############################################################################
#
# Add plugins ...
#
###############################################################################

# AWS CLI ...
#------------------------------------------------------------------------------
# > pip3 install awscli --upgrade --user
# This should be sourced after compinit above
source $(which aws_zsh_completer.sh)

# K ...
#------------------------------------------------------------------------------
# Various colorizations, type 'k', instead of 'ls' or alias 'l' ...
# Extra column for git notation ...
source ${MY_SUITCASE_ZSH_PLUGINS}/k/k.plugin.zsh

# Fast Syntax Highlighting ...
#------------------------------------------------------------------------------
# Typed text is colored to validate command spelling ...
source ${MY_SUITCASE_ZSH_PLUGINS}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

# Prioritize history by cwd ...
#------------------------------------------------------------------------------
# Command history is biased by cwd
#source ${MY_SUITCASE_ZSH_PLUGINS}/zsh-prioritize-cwd-history/zsh-prioritize-cwd-history.zsh


# Prediction ...
#------------------------------------------------------------------------------
# Shell will predict cmd in grey text,
# * Type <ctrl-e> to accept and move cursor to end-of-line.
# * Or continue typing to ignore
# (Reference: GNU readline keybindings)
source ${MY_SUITCASE_ZSH_PLUGINS}/zsh-autosuggestions/zsh-autosuggestions.zsh

# Oh-My-Zsh collection ...
#------------------------------------------------------------------------------
#
# ** CAUTION: **
#    These plugins potentially import a lot of crappy aliases!
#    Use sparingly, unless you like all the added aliases.

source ${MY_SUITCASE_ZSH_PLUGINS}/oh-my-zsh/plugins/colored-man-pages/colored-man-pages.plugin.zsh

###############################################################################
#
# Import configurations ...
#
###############################################################################

# TODO: env 
# Files in private should be ignored by git
foreach config (
    aliases \
    functions \
    options \
    variables \
    zstyle \
    private/aliases \
    private/functions \
    private/variables 
)
do
    file="${MY_SUITCASE_ZSH_HOME}/configs/${config}.zsh"

    if [ ! -e "$file" ]; then
         echo -e "** WARNING **  zsh config could not find your $file\a"
    elif [ ! -r "$file" ]; then
    	echo -e "** WARNING **  zsh config could not read your $file:\a"
    else
     	source $file
    fi
done

###############################################################################
#
# Add theme ...
#
###############################################################################

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

MY_SUITCASE_ZSH_THEME=${MY_SUITCASE_ZSH_PLUGINS}/scm/github/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f $MY_SUITCASE_ZSH_THEME ]] || source $MY_SUITCASE_ZSH_THEME

# To recreate and customize prompt, run `p10k configure` => ~/.p10k.zsh
MY_SUITCASE_ZSH_THEME_CONFIG=${MY_SUITCASE_ZSH_HOME}/themes/p10k.zsh
[[ ! -f $MY_SUITCASE_ZSH_THEME_CONFIG ]] || source $MY_SUITCASE_ZSH_THEME_CONFIG

###############################################################################
