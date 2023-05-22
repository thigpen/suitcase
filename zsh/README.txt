
ZSH configuraiton of Powerline 10k inside iTerm, with "MeslGS NF font"

1) Install font

Double-click the font file.

2) [Optional] Create a new profile in iTerm

Preferences > Profile > +

3) In profile, set both "Font" and "Non-ASCII Font" to MesloGS NF"

4) Download plugins from both HomeBrew and GIT

> cd plugins/scm
> ./download_plugins.sh

5) Install some HomeBrew packages

> brew install osx-cpu-temp
> brew install zsh-completions
> brew install coreutils

5) Set the shell config file

> ln -s ~/suitcase/zsh/zshrc.zsh ~/.zshrc

6) Execute zsh

> zsh

WARNING: You can eventually set zsh to your default login shell. But
until you've worked out all the errors, you might not want to make
your default shell, a bOrken shell.
