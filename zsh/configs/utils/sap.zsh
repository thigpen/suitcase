
function aws_profiles() {
  [[ -r "${AWS_CONFIG_FILE:-$HOME/.aws/config}" ]] || return 1
  grep --color=never -Eo '^\[.*\]' "${AWS_CONFIG_FILE:-$HOME/.aws/config}" | sed -E 's/^[[:space:]]*\[(profile)?[[:space:]]*([-_[:alnum:]\.@]+)\][[:space:]]*$/\2/g'
}

function _aws_profiles() {
  reply=($(aws_profiles))
}

function aws_credentials() {
  [[ -r "${AWS_CONFIG_FILE:-$HOME/.aws/credentials}" ]] || return 1
  grep --color=never -Eo '^\[.*\]' "${AWS_CONFIG_FILE:-$HOME/.aws/credentials}" | sed -E 's/^[[:space:]]*\[(profile)?[[:space:]]*([-_[:alnum:]\.@]+)\][[:space:]]*$/\2/g'
}

function set-aws-profile-and-creds() {
  if [[ -z "$1" ]]; then
    unset AWS_DEFAULT_PROFILE AWS_PROFILE AWS_REGION AWS_EB_PROFILE AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY
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
    local -a available_credentials
    available_credentials=($(aws_credentials))
    if [[ -z "${available_credentials[(r)$1]}" ]]; then
      echo "${fg[red]}Profile '$1' not found in '${AWS_CONFIG_FILE:-$HOME/.aws/credentials}'" >&2
      echo "Available profiles: ${(j:, :)available_profiles:-no profiles found}${reset_color}" >&2
      return 1
    fi
  fi

  if [ $OKTA ]; then
    xaws $1  # Sourced from ~/.gimme-aws-creds-xaws
             # via https://github.sie.sony.com/sie/gimme-aws-creds
  else
    local aws_profile=$1
    profile_data=$(cat ~/.aws/credentials | grep "\[$aws_profile\]" -A4)   # Weakness: assumes block of 4 lines ...
    AWS_ACCESS_KEY_ID="$(echo $profile_data | grep aws_access_key_id | cut -f2 -d'=' | tr -d ' ')"
    AWS_SECRET_ACCESS_KEY="$(echo $profile_data | grep aws_secret_access_key | cut -f2 -d'=' | tr -d ' ')"
    config_data=$(cat ~/.aws/config | grep "\[profile $aws_profile\]" -A4)
    AWS_REGION="$(echo $config_data | grep region | cut -f2 -d'=' | tr -d ' ')"
    #set -x
    export AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID
    export AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY
    export AWS_REGION=$AWS_REGION
    #set +x
  fi
  
  export AWS_DEFAULT_PROFILE=$1
  export AWS_PROFILE=$1
  export AWS_EB_PROFILE=$1
}

compctl -K _aws_profiles xaws set-aws-profile-and-creds

