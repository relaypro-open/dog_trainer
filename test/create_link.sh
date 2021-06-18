#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

source test_lib.sh
TESTDIR=/opt/dog_trainer/test

echo "LINK/EXTERNAL"
HOST_ID=$(post "${TESTDIR}/link.json" "${BASEURL}/link")

echo
echo "PASS: ${PASS} / FAIL: ${FAIL}"
