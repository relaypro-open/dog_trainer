#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

PASS=0
FAIL=0
source test_lib.sh
TESTDIR=.
BASEURL=http://localhost:7070/api/V2

echo "dog_trainer basic API V2 test"

echo "ZONE"
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"2.2.2.2\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone.json
ZONE_ID=$(post "${TESTDIR}/zone.json" "${BASEURL}/zone")
#echo "ZONE_ID: ${ZONE_ID}"
get ${BASEURL}/zone/${ZONE_ID}
get ${BASEURL}/zone?name=drewtest
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"3.3.3.3\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone_update.json
put "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}"
putc "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}" 303
get ${BASEURL}/zone/${ZONE_ID}
delete ${BASEURL}/zone/${ZONE_ID}
get ${BASEURL}/zones

echo "SERVICE"
SERVICE_ID=$(post "${TESTDIR}/service.json" "${BASEURL}/service")
get ${BASEURL}/service/${SERVICE_ID} 
get ${BASEURL}/service?name=drewtest
echo -e "{\n\"name\":\"drewtest2\"\n}" > ${TESTDIR}/service_update.json
put "${TESTDIR}/service_update.json" "${BASEURL}/service/${SERVICE_ID}"
putc "${TESTDIR}/service_update.json" "${BASEURL}/service/${SERVICE_ID}" 303
putc "${TESTDIR}/service_update_no_ports.json" "${BASEURL}/service/${SERVICE_ID}" 303
putc "${TESTDIR}/service_update_string_ports.json" "${BASEURL}/service/${SERVICE_ID}" 400
get ${BASEURL}/service/${SERVICE_ID}
delete ${BASEURL}/service/${SERVICE_ID}
get ${BASEURL}/services

#ssh service
#SERVICE_ID=$(curl -d "${TESTDIR}/service_any.json" -H "Content-Type: application/json" -X POST "${BASEURL}/service" | jq -r .id)
SERVICE_ID=$(post "${TESTDIR}/service_ssh.json" "${BASEURL}/service")

echo "RULESET"
RULESET_ID=$(post "${TESTDIR}/ruleset.json" "${BASEURL}/ruleset")
get ${BASEURL}/ruleset/${RULESET_ID}
get ${BASEURL}/ruleset?name=drewtest
put "${TESTDIR}/ruleset_update.json" "${BASEURL}/ruleset/${RULESET_ID}"
putc "${TESTDIR}/ruleset_update.json" "${BASEURL}/ruleset/${RULESET_ID}" 303 #updates create new rulesets
get ${BASEURL}/ruleset/${RULESET_ID}
get ${BASEURL}/rulesets
#RULESET_ID="604ade9a-57da-4e4a-865f-5116e4b28a77"

echo "PROFILE"
cat profile_v2.json.template | sed 's/${RULESET_ID}/'${RULESET_ID}'/g' > profile_v2.json
cat profile_v2_update.json.template | sed 's/${RULESET_ID}/'${RULESET_ID}'/g' > profile_v2_update.json
PROFILE_ID=$(post "${TESTDIR}/profile.json" "${BASEURL}/profile")
get ${BASEURL}/profile/${PROFILE_ID}
get ${BASEURL}/profile?name=drewtest
put "${TESTDIR}/profile_v2_update.json" "${BASEURL}/profile/${PROFILE_ID}"
putc "${TESTDIR}/profile_v2_update.json" "${BASEURL}/profile/${PROFILE_ID}" 303 #updates create new profiles
putc "${TESTDIR}/profile_v2_update_blank_name.json" "${BASEURL}/profile/${PROFILE_ID}" 400
get ${BASEURL}/profile/${PROFILE_ID}
delete ${BASEURL}/profile/${PROFILE_ID}
get ${BASEURL}/profiles

echo "RULESET CLEANUP"
delete ${BASEURL}/ruleset/${RULESET_ID}

echo "GROUP"
GROUP_ID=$(post "${TESTDIR}/group.json" "${BASEURL}/group")
get ${BASEURL}/group/${GROUP_ID}
get "${BASEURL}/group?name=drewtest"
put "${TESTDIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}"
putc "${TESTDIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}" 303
putc "${TESTDIR}/group_update_blank_name.json" "${BASEURL}/group/${GROUP_ID}" 400
get ${BASEURL}/group/${GROUP_ID}
delete ${BASEURL}/group/${GROUP_ID}
get ${BASEURL}/groups

echo "HOST"
HOST_ID=$(post "${TESTDIR}/host.json" "${BASEURL}/host")
get ${BASEURL}/host/${HOST_ID}
get "${BASEURL}/host?hostkey=drewtest-phoneboothdev-info"
put "${TESTDIR}/host_update.json" "${BASEURL}/host/${HOST_ID}"
putc "${TESTDIR}/host_update.json" "${BASEURL}/host/${HOST_ID}" 303
putc "${TESTDIR}/host_update_blank_hostname.json" "${BASEURL}/host/${HOST_ID}" 400
get ${BASEURL}/host/${HOST_ID}
delete ${BASEURL}/host/${HOST_ID}
get ${BASEURL}/hosts

echo "LINK/EXTERNAL"
HOST_ID=$(post "${TESTDIR}/link.json" "${BASEURL}/link")
get ${BASEURL}/link/${HOST_ID}
get "${BASEURL}/link?name=x1"
EXTERNAL_ID=$(get_id "${BASEURL}/external?name=x1")
put "${TESTDIR}/link_update.json" "${BASEURL}/link/${HOST_ID}"
get ${BASEURL}/link/${HOST_ID}
delete ${BASEURL}/link/${HOST_ID}
getc "${BASEURL}/link?name=x1" 404
getc "${BASEURL}/external?name=x1" 404
get ${BASEURL}/links

echo "FACT"
FACT_ID=$(post "${TESTDIR}/fact.json" "${BASEURL}/fact")
get ${BASEURL}/fact/${FACT_ID}
get "${BASEURL}/fact?name=dev_qa"
put "${TESTDIR}/fact_update.json" "${BASEURL}/fact/${FACT_ID}"
putc "${TESTDIR}/fact_update.json" "${BASEURL}/fact/${FACT_ID}" 303
putc "${TESTDIR}/fact_update_blank_factname.json" "${BASEURL}/fact/${FACT_ID}" 400
get ${BASEURL}/fact/${FACT_ID}
delete ${BASEURL}/fact/${FACT_ID}
get ${BASEURL}/facts

delete ${BASEURL}/service/${SERVICE_ID}
echo
#echo "PASS: ${PASS} / FAIL: ${FAIL}"
test_report
