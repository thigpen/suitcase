
###############################################################################
# Functions ...

addpath()   { path=($path:$1) } 
prepath()   { path=($1:$path) }
rmpath()                                # Remove from path
{                                       # Depending on tab complete ...
    dir=`echo $1 | sed 's/\/$//'`       #   Remove finicky trailing slash 
    path=(${(R)path:#$dir})             # Param expansion ...
                                        #   ${name:#patt} replace with null
}

funtion keb() { /bin/rm -i *~; /bin/rm -i \#* }   # Kill emacs backups

################################################################################

if [[ $OSTYPE = darwin* ]];
then
    # Turn OS X hidden files on/off in Finder
    function mzf_hiddenOn()
    { defaults write com.apple.Finder AppleShowAllFiles YES ; }
    
    function mzf_hiddenOff()
    { defaults write com.apple.Finder AppleShowAllFiles NO ; }

    # View man pages in OS X Preview
    function mzf_manpageToPreview()
    { ps=`mktemp -t manpageXXXX`.ps ; man -t $@ > "$ps" ; open "$ps" ; }
fi

################################################################################

# Display disk mounts 
function mzf_niceMount()
{ (echo "DEVICE PATH TYPE FLAGS" && mount | awk '$2="";1') | column -t ; }

# Display table of ANSI terminal colors ...
function colortable() {
  export esc="\033["
  echo -e "\t  40\t   41\t   42\t    43\t      44       45\t46\t 47"
  for fore in 30 31 32 33 34 35 36 37; do
    line1="$fore  " line2="    "
    for back in 40 41 42 43 44 45 46 47; do
      line1="${line1}${esc}${back};${fore}m Normal  ${esc}0m"
      line2="${line2}${esc}${back};${fore};1m Bold    ${esc}0m"
    done
    echo -e "$line1\n$line2"
  done
}

# Copy the file "$1" as "$1.bak"
function mzf_copyToBak()
{ cp $1{,.bak} }

# myIP address
function mzf_showIp() {
    ifconfig lo0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "lo0       : " $2}'
    ifconfig en0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
    ifconfig en0 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en0 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
    ifconfig en1 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en1 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
    ifconfig en1 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en1 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
}

###############################################################################

function my-non-docker-desktop()
{
    minikube start
    eval $(minikube docker-env)
    echo "`minikube ip` docker.local" | sudo tee -a /etc/hosts > /dev/null
}

################################################################################
