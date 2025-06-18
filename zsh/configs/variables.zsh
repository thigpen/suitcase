HISTFILE=~/.zsh_history         # where to store zsh config
HISTSIZE=1024                   # big history
SAVEHIST=1024                   # big history

PAGER='less'
EDITOR='emacs'

PREPROMPT="%B<%b %(80#..%n@)%m %B%~%b %(0?.. %B<err: %?>%b) %B>%b"
PS1='%B]%b ';
RPS1=$'%B<%b %D{%a %b/%e/%G %H:%M:%S} %B>%b'

PATH=$HOME/bin:$PATH

AKAMAI_PRAGMA_HEADERS="Pragma: akamai-x-cache-on, akamai-x-cache-remote-on, akamai-x-check-cacheable, akamai-x-get-cache-key, akamai-x-get-extracted-values, akamai-x-get-ssl-client-session-id, akamai-x-get-true-cache-key, akamai-x-serial-no, akamai-x-get-request-id,akamai-x-get-nonces,akamai-x-get-client-ip,akamai-x-feo-trace"

# For Kapeli Dash app ...
export MANPATH="${MANPATH}:${HOME}/workbench/soft/share/linux-man/usr/share/man/"

# TODO: Temp hack to STILL support python2 and pip ...
#PATH=$PATH:/Users/hhughes/Library/Python/2.7/bin

################################################################################
#
# Setup private software tree called ~/soft ...
#
################################################################################

# https://www.usenix.org/legacy/publications/library/proceedings/lisa94/full_papers/leslie1.ps
#
# For OX X Catalina +
# > man 5 synthetcic.conf
# > sudo echo "soft\tSystem/Volumes/Data/soft\n" > /etc/synthetic.conf && shutdown -r now

ARCH=$(arch)

for dir_target in ~/soft/arch/${ARCH}/bin ~/soft/share/bin ~/soft/homemade/bin
do
    [ -d $dir_target ] && PATH=$dir_target:$PATH
done

###############################################################################
#
# GNU ...

[[ -d $MY_SUITCASE_BREW_GNU_BIN ]] && PATH=$MY_SUITCASE_BREW_GNU_BIN:$PATH

###############################################################################
#
# JDK ...
[[ -d /opt/homebrew/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home/ ]] \
    && export JAVA_HOME=/opt/homebrew/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home/ \
    && PATH=$JAVA_HOME/bin:$PATH

###############################################################################
#
# KUBECONFIG ...

[[ -d ~/.kube/config ]] && KUBECONFIG=$KUBECONFIG:~/.kube/config


################################################################################
#
# Setup language frameworks ...
#
################################################################################

#-------------------------------------------------------------------------------
# Go ...
#-------------------------------------------------------------------------------
if type go &>/dev/null; then
    export GOPATH=$(go env GOPATH)
    NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
fi

#-------------------------------------------------------------------------------
# GroovySDK ...
#-------------------------------------------------------------------------------
if type groovy &>/dev/null; then
    if [ -d /usr/local/opt/groovysdk/libexec ]; then
	export GROOVY_HOME=/usr/local/opt/groovysdk/libexec
    fi
fi

#-------------------------------------------------------------------------------
# Ruby ...
#-------------------------------------------------------------------------------
[ -d /opt/homebrew/opt/ruby/bin ]            && PATH=/opt/homebrew/opt/ruby/bin:$PATH
[ -d /opt/homebrew/lib/ruby/gems/3.0.0/bin ] && PATH=/opt/homebrew/lib/ruby/gems/3.0.0/bin:$PATH

[ -d /usr/local/opt/ruby/bin ]               && PATH=/usr/local/opt/ruby/bin:$PATH

# TODO: From brew
[ -d /opt/homebrew/bin ]                     && PATH=/opt/homebrew/bin:$PATH
#-------------------------------------------------------------------------------
# Terraform ...
#-------------------------------------------------------------------------------
# https://www.terraform.io/docs/configuration/environment-variables.html
if type terraform &>/dev/null; then
    # See ~/.terraformrc

    #export TF_LOG=TRACE           # {TRACE,DEBUG,INFO,WARN,ERROR}
    #export TF_LOG_PATH=~/.terraform.d/terraform.log
    #export TF_INPUT=              # {false,0} -input=false
    #export TF_MODULE_DEPTH=2      # -module-depth=VALUE

    # export TF_VAR_name
    # export TF_CLI_ARGS
    # export TF_CLI_ARGS_name
    # export TF_DATA_DIR
    # export TF_SKOP_REMOTE_TEST
fi

#-------------------------------------------------------------------------------
# Docker ...
#-------------------------------------------------------------------------------
if type terraform &>/dev/null; then
    DOCKER_SLEEP='/bin/sh -c "while true; do sleep 10; done"'
fi

#-------------------------------------------------------------------------------
# OpenJDK ...
#-------------------------------------------------------------------------------
[ -d /usr/local/opt/openjdk@11/bin ] && PATH=/usr/local/opt/openjdk@11/bin:$PATH

#-------------------------------------------------------------------------------
# Astral UV ...
#-------------------------------------------------------------------------------

# For Astral uv (python tools) ...
[ -d $HOME/.local/bin ] && PATH=$HOME/.local/bin:$PATH

################################################################################
#
# Setup operating systems ...
#
################################################################################

#if [[ $OSTYPE = darwin* ]];
#then
#
#fi

#if [[ $OSTYPE = aix* ]];
#then
#    export WS=/usr/WebSphere5/AppServer/
#    export WSI=${WS}/installedApps/
#    export WSL=${WS}/logs/appslog/
#fi

#if [[ $OSTYPE = solaris* ]];
#then
#
#fi

################################################################################

# Filter repeats ...
typeset -U path manpath

################################################################################
