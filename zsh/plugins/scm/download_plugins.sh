#!/bin/sh

DISTRIB_DIR=github

mkdir -p $DISTRIB_DIR

cd $DISTRIB_DIR

repos="https://github.com/weizard/assume-role.git \
    https://github.com/zdharma/fast-syntax-highlighting.git \
    https://github.com/supercrabtree/k \
    https://github.com/robbyrussell/oh-my-zsh \
    https://github.com/romkatv/powerlevel10k.git \
    https://github.com/zsh-users/zsh-autosuggestions \
    https://github.com/zsh-users/zsh-completions"

for repo in $repos
do 
    git clone $repo
done

mkdir -p docker
curl -o docker/_docker https://raw.githubusercontent.com/docker/cli/master/contrib/completion/zsh/_docker
