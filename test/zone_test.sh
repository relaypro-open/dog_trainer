#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

source test_lib.sh
TESTDIR=/opt/dog_trainer/test

echo "dog_trainer basic API test"
#echo -e "{\n\"name\":\"office\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"2.2.2.2\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone.json
#ZONE_ID=$(curl -d "${TESTDIR}/zone.json' --silent -H "Content-Type: application/json" -X POST "${BASEURL}/zone" | jq -r .id)
echo "ZONE"
echo -e "{\n\"name\":\"cpz_build_slave\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"2.2.2.2\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone.json
ZONE_ID="0f3d89d7-6f6d-4e74-8d47-a99197bfc84e"
put "${TESTDIR}/zone.json" "${BASEURL}/zone/${ZONE_ID}"
#echo "ZONE_ID: ${ZONE_ID}"
#get ${BASEURL}/zone/${ZONE_ID}
#get ${BASEURL}/zone?name=cpz_build_slave
echo -e "{\n\"name\":\"cpz_build_slave\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"3.3.3.3\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone_update.json
put "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}"
#putc "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}" 303
#get ${BASEURL}/zone/${ZONE_ID}
#delete ${BASEURL}/zone/${ZONE_ID}
#get ${BASEURL}/zones

echo
echo "PASS: ${PASS} / FAIL: ${FAIL}"
