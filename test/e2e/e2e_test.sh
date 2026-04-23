#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

# E2E Test - Based on api_v2_test.sh
# Verifies API V2 CRUD and agent behavior

export BASEURL="http://localhost:7070/api/V2"
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
source "${SCRIPT_DIR}/e2e_lib.sh"
cd "${SCRIPT_DIR}"
TEST_DATA_DIR=".."

echo "dog_trainer E2E API V2 test"

# 0. Discover IPs for E2E
DOG_AGENT_IP=$(get_agent_ip "dog-agent-1")
DOG_AGENT_2_IP=$(get_agent_ip "dog-agent-2")
echo "dog-agent-1 IP: $DOG_AGENT_IP"
echo "dog-agent-2 IP: $DOG_AGENT_2_IP"

echo "INITIAL SETUP FOR AGENT REGISTRATION"
# Agents come up requesting "local_group". We must ensure this group exists
# and has a valid profile/ruleset so the agents get configured immediately upon registration.
LOCAL_PROFILE_ID=$(post <(echo '{"name":"local_group","version":"1.0"}') "${BASEURL}/profile")
resolve_json_template "ruleset_local_group.json.template" "ruleset_local_group.json" "__PROFILE_ID__" "$LOCAL_PROFILE_ID"
LOCAL_RULESET_ID=$(post "ruleset_local_group.json" "${BASEURL}/ruleset")
LOCAL_GROUP_ID=$(post <(echo "{\"name\":\"local_group\",\"profile_name\":\"local_group\",\"profile_version\":\"latest\"}") "${BASEURL}/group")

#echo "Restarting agents so they register against the now-valid local_group..."
#docker container restart dog-agent-1 dog-agent-2
#DOG_AGENT_IP=$(get_agent_ip "dog-agent-1")
#DOG_AGENT_2_IP=$(get_agent_ip "dog-agent-2")
#echo "dog-agent-1 IP (post-restart): $DOG_AGENT_IP"
#echo "dog-agent-2 IP (post-restart): $DOG_AGENT_2_IP"

echo "ZONE"
# Inject agent IPs into zone.json
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"$DOG_AGENT_IP\",\"$DOG_AGENT_2_IP\"],\n\"ipv6_addresses\":[]\n}" > zone.json
ZONE_ID=$(post "zone.json" "${BASEURL}/zone")
get ${BASEURL}/zone/${ZONE_ID}
get ${BASEURL}/zone?name=drewtest
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"$DOG_AGENT_IP\",\"$DOG_AGENT_2_IP\",\"3.3.3.3\"],\n\"ipv6_addresses\":[]\n}" > zone_update.json
put "zone_update.json" "${BASEURL}/zone/${ZONE_ID}"
putc "zone_update.json" "${BASEURL}/zone/${ZONE_ID}" 303
get ${BASEURL}/zone/${ZONE_ID}

echo "SERVICE"
SERVICE_ID=$(post "${TEST_DATA_DIR}/service.json" "${BASEURL}/service")
get ${BASEURL}/service/${SERVICE_ID}
get ${BASEURL}/service?name=drewtest
echo -e "{\n\"name\":\"drewtest2\"\n}" > service_update.json
put "service_update.json" "${BASEURL}/service/${SERVICE_ID}"
putc "service_update.json" "${BASEURL}/service/${SERVICE_ID}" 303
putc "${TEST_DATA_DIR}/service_update_no_ports.json" "${BASEURL}/service/${SERVICE_ID}" 303
putc "${TEST_DATA_DIR}/service_update_string_ports.json" "${BASEURL}/service/${SERVICE_ID}" 400
get ${BASEURL}/service/${SERVICE_ID}

# EPMD service for connectivity tests
echo '{"name":"test-epmd","services":[{"ports":["4369"],"protocol":"tcp"}],"version":1}' > service_epmd.json
EPMD_SERVICE_ID=$(post "service_epmd.json" "${BASEURL}/service")

echo "PROFILE"
PROFILE_ID=$(post <(echo '{"name":"drewtest","version":"1.1"}') "${BASEURL}/profile")
get ${BASEURL}/profile/${PROFILE_ID}
get ${BASEURL}/profile?name=drewtest
put <(echo '{"name":"drewtest2","version":"1.1"}') "${BASEURL}/profile/${PROFILE_ID}"
putc <(echo '{"name":"drewtest2","version":"1.1"}') "${BASEURL}/profile/${PROFILE_ID}" 303
get ${BASEURL}/profile/${PROFILE_ID}

echo "GROUP"
GROUP_ID=$(post <(echo '{"name":"drewtest"}') "${BASEURL}/group")
get ${BASEURL}/group/${GROUP_ID}
get "${BASEURL}/group?name=drewtest"
put "${TEST_DATA_DIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}"
putc "${TEST_DATA_DIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}" 303
putc "${TEST_DATA_DIR}/group_update_blank_name.json" "${BASEURL}/group/${GROUP_ID}" 400
get ${BASEURL}/group/${GROUP_ID}

echo "RULESET"
# Build rulesets with actual entity IDs — groups, zones, and services referenced by ID
resolve_json_template "ruleset_epmd.json.template" "ruleset_epmd.json" \
    "__PROFILE_ID__" "$PROFILE_ID" \
    "__GROUP_ID__" "$GROUP_ID" \
    "__ZONE_ID__" "$ZONE_ID" \
    "__SERVICE_ID__" "$EPMD_SERVICE_ID" \
    "__LOG__" "false"

RULESET_ID=$(post "ruleset_epmd.json" "${BASEURL}/ruleset")
get ${BASEURL}/ruleset/${RULESET_ID}
get ${BASEURL}/ruleset?name=drewtest

resolve_json_template "ruleset_epmd.json.template" "ruleset_epmd_update.json" \
    "__PROFILE_ID__" "$PROFILE_ID" \
    "__GROUP_ID__" "$GROUP_ID" \
    "__ZONE_ID__" "$ZONE_ID" \
    "__SERVICE_ID__" "$EPMD_SERVICE_ID" \
    "__LOG__" "true"

put "ruleset_epmd_update.json" "${BASEURL}/ruleset/${RULESET_ID}"
putc "ruleset_epmd_update.json" "${BASEURL}/ruleset/${RULESET_ID}" 303
get ${BASEURL}/ruleset/${RULESET_ID}

echo "HOST"
# Registration and assignment
HOST_ID=$(wait_for_host_registered "dog-agent-1" 120)
echo "HOST_ID: $HOST_ID"
get ${BASEURL}/host/${HOST_ID}
# Update host
put "host_update.json" "${BASEURL}/host/${HOST_ID}"
putc "host_update.json" "${BASEURL}/host/${HOST_ID}" 303
get ${BASEURL}/host/${HOST_ID}

echo "LINK/EXTERNAL"
LINK_ID=$(post "${TEST_DATA_DIR}/link.json" "${BASEURL}/link")
get ${BASEURL}/link/${LINK_ID}
get "${BASEURL}/link?name=x1"
EXTERNAL_ID=$(get_id "${BASEURL}/external?name=x1")
put "${TEST_DATA_DIR}/link_update.json" "${BASEURL}/link/${LINK_ID}"
get ${BASEURL}/link/${LINK_ID}

echo "FACT"
FACT_ID=$(post "${TEST_DATA_DIR}/fact.json" "${BASEURL}/fact")
get ${BASEURL}/fact/${FACT_ID}
get "${BASEURL}/fact?name=dev_qa"
put "${TEST_DATA_DIR}/fact_update.json" "${BASEURL}/fact/${FACT_ID}"
putc "${TEST_DATA_DIR}/fact_update.json" "${BASEURL}/fact/${FACT_ID}" 303
putc "${TEST_DATA_DIR}/fact_update_blank_factname.json" "${BASEURL}/fact/${FACT_ID}" 400
get ${BASEURL}/fact/${FACT_ID}

echo "--- E2E BEHAVIORAL VERIFICATION ---"

# To verify behavioral convergence, we need to:
# 1. Have a group that uses a profile.
# 2. Have a host in that group.
# 3. Have rules in that profile that allow/deny traffic.

# Behavioral ruleset/profile/group all share the name "test"
BEHAVIORAL_PROFILE_ID=$(post <(echo '{"name":"test","version":"1.0"}') "${BASEURL}/profile")

# "test" ROLE group — ruleset allows EPMD inbound from this group and "office" ZONE
TEST_ROLE_GROUP_ID=$(post <(echo '{"name":"test"}') "${BASEURL}/group")

resolve_json_template "ruleset_test.json.template" "ruleset_test.json" \
    "__PROFILE_ID__" "$BEHAVIORAL_PROFILE_ID" \
    "__TEST_GROUP_ID__" "$TEST_ROLE_GROUP_ID" \
    "__OFFICE_ZONE_ID__" "$ZONE_ID" \
    "__EPMD_SERVICE_ID__" "$EPMD_SERVICE_ID"

BEHAVIORAL_RULESET_ID=$(post "ruleset_test.json" "${BASEURL}/ruleset")

# Assign the EPMD profile to the test group (profile_name matches group name)
put <(echo "{\"name\":\"test\",\"profile_name\":\"test\",\"profile_version\":\"latest\"}") "${BASEURL}/group/${TEST_ROLE_GROUP_ID}"

get ${BASEURL}/host/${HOST_ID}
# Assign host to the test group
put "host_update_e2e.json" "${BASEURL}/host/${HOST_ID}"

#sleep 15
#echo "Restarting agents so they register with the new config..."
#docker container restart dog-agent-1 dog-agent-2
#DOG_AGENT_IP=$(get_agent_ip "dog-agent-1")
#DOG_AGENT_2_IP=$(get_agent_ip "dog-agent-2")
#echo "dog-agent-1 IP (post-restart): $DOG_AGENT_IP"
#echo "dog-agent-2 IP (post-restart): $DOG_AGENT_2_IP"

# Agent re-registers in local_group on restart; reassign to test group
#HOST_ID=$(wait_for_host_registered "dog-agent-1" 120)
#put "host_update_e2e.json" "${BASEURL}/host/${HOST_ID}"

# Update zone to 'office' so the other agent is allowed
put <(echo "{\"name\":\"office\",\"ipv4_addresses\":[\"$DOG_AGENT_IP\",\"$DOG_AGENT_2_IP\"],\"ipv6_addresses\":[]}") "${BASEURL}/zone/${ZONE_ID}"

echo "Waiting for convergence..."
wait_for_convergence "${TEST_ROLE_GROUP_ID}" "${HOST_ID}" 300

echo "Verifying Connectivity (Expected: OPEN for EPMD)..."
assert_connectivity "dog-agent-2" "$DOG_AGENT_IP" 4369 "open"

echo "CLEANUP (Skipped if KEEP_RUNNING=1)"
if [[ "${KEEP_RUNNING:-0}" -eq 0 ]]; then
    delete ${BASEURL}/fact/${FACT_ID}
    delete ${BASEURL}/link/${LINK_ID}
    delete ${BASEURL}/host/${HOST_ID}
    delete ${BASEURL}/group/${GROUP_ID}
    delete ${BASEURL}/group/${TEST_ROLE_GROUP_ID}
    delete ${BASEURL}/group/${LOCAL_GROUP_ID}
    delete ${BASEURL}/profile/${PROFILE_ID}
    delete ${BASEURL}/profile/${LOCAL_PROFILE_ID}
    delete ${BASEURL}/profile/${BEHAVIORAL_PROFILE_ID}
    delete ${BASEURL}/ruleset/${BEHAVIORAL_RULESET_ID}
    delete ${BASEURL}/ruleset/${RULESET_ID}
    delete ${BASEURL}/ruleset/${LOCAL_RULESET_ID}
    delete ${BASEURL}/service/${SERVICE_ID}
    delete ${BASEURL}/service/${EPMD_SERVICE_ID}
    delete ${BASEURL}/zone/${ZONE_ID}

    rm -f zone.json zone_update.json service_update.json service_epmd.json ruleset_epmd.json ruleset_epmd_update.json ruleset_test.json host_update_e2e.json
fi

echo
test_report
