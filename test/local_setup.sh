#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

PASS=0
FAIL=0
source test_lib.sh
TESTDIR=.
BASEURL=http://localhost:7070/api/V2

echo "dog_trainer basic local setup"


echo "RULESET"
RULESET_ID=$(post "${TESTDIR}/local_ruleset.json" "${BASEURL}/ruleset")
get ${BASEURL}/ruleset/${RULESET_ID}

echo "PROFILE"
PROFILE_ID=$(post "${TESTDIR}/local_profile_v2.json" "${BASEURL}/profile")
get ${BASEURL}/profile/${PROFILE_ID}

echo "GROUP"
GROUP_ID=$(post "${TESTDIR}/local_group.json" "${BASEURL}/group")
get ${BASEURL}/group/${GROUP_ID}
