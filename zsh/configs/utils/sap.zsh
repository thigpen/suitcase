
function aws_profiles() {
  [[ -r "${AWS_CONFIG_FILE:-$HOME/.aws/config}" ]] || return 1
  grep --color=never -Eo '^\[.*\]' "${AWS_CONFIG_FILE:-$HOME/.aws/config}" | sed -E 's/^[[:space:]]*\[(profile)?[[:space:]]*([-_[:alnum:]\.@]+)\][[:space:]]*$/\2/g'
}

function _aws_profiles() {
  reply=($(aws_profiles))
}

function sap() {
  if [[ -z "$1" ]]; then
    unset AWS_DEFAULT_PROFILE AWS_PROFILE AWS_EB_PROFILE
    echo AWS profile cleared.
    return
  else
    local -a available_profiles
    available_profiles=($(aws_profiles))
    if [[ -z "${available_profiles[(r)$1]}" ]]; then
      echo "${fg[red]}Profile '$1' not found in '${AWS_CONFIG_FILE:-$HOME/.aws/config}'" >&2
      echo "Available profiles: ${(j:, :)available_profiles:-no profiles found}${reset_color}" >&2
      return 1
    fi
  fi

  xaws Aloy # Sourced from ~/.gimme-aws-creds-xaws
            # via https://github.sie.sony.com/sie/gimme-aws-creds
  
  export AWS_DEFAULT_PROFILE=$1
  export AWS_PROFILE=$1
  export AWS_EB_PROFILE=$1
}

compctl -K _aws_profiles xaws sap

