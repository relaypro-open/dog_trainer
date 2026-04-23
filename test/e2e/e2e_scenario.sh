#!/bin/bash
set -x
set -euo pipefail
IFS=$'\n\t'

export PASS=0
export FAIL=0

source "$(dirname "$0")/../test_lib.sh"
export BASEURL="http://localhost:7070/api/V2"

echo "E2E SCENARIO"
curl --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/host" | jq .
