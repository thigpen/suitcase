function def_real_alias () {
  while [[ $# -ge 2 ]]; do
    alias "$1=$2"
    shift 2
  done
}

function def_real_aliases () {
  def_real_alias $real_aliases
  unset real_aliases
}

function def_global_alias () {
  while [[ $# -ge 2 ]]; do
    alias -g "$1=$2"
    shift 2
  done
}

function def_global_aliases () {
  def_global_alias $global_aliases
  unset global_aliases
}

function def_fun () {
  while [[ $# -ge 2 ]]; do
    eval "function $1 () { $2 \$@ }"
    shift 2
  done
}

function def_funs () {
  def_fun $funs
  unset funs
}






real_aliases=(
  meh "echo meh"
); def_real_aliases

global_aliases=(
  :: ':>!'

  B '`git rev-parse --abbrev-ref HEAD`'

  V "|& less"
  G "|& egrep --color"
  S "|& sort"
  R "|& sort -rn"
  L "|& wc -l | sed 's/^\ *//'"

  H  "|& head"
  T  "|& tail"
  H1 "H -n 1"
  T1 "T -n 1"

  ZF '*(.L0)'     # zero-length regular files
  ZD '*(/L0)'     # zero-length directories

  AE '{,.}*'      # all files, including dot files
  AF '**/*(.)'    # all regular files
  AD '**/*(/)'    # all directories
  AS '**/*(@)'    # all symlinks

  OF '*(.om[-1])' # oldest regular file
  OD '*(/om[-1])' # oldest directory
  OS '*(@om[-1])' # oldest symlink

  NF '*(.om[1])'  # newest regular file
  ND '*(/om[1])'  # newest directory
  NS '*(@om[1])'  # newest symlink

); def_global_aliases

funs=(
  z "exec zsh"
  s "sudo"

  d "pushd"
  \- "popd"
  ds "dirs -l"

  cp "command cp -i"
  mv "command mv -i"

  l "ls -GFAtr --color"
  la "ls -AF --color"
  ll "l -l"
  l1 "l -1"
  lh "l -H"
  lr "l -R"
  lk "la -l"

  sl "ln -sf"
  md "mkdir -p"

  f "fd"
  g "ripgrep --color auto"
  gi "g -i"
  tf "tail -F"
  rh "rehash"

  mount "s mount"
  umount "s umount"
  reboot "s reboot"
  poweroff "s poweroff"
  halt "s halt -p"
); def_funs
