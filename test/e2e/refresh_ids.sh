#!/bin/bash
set -euo pipefail

BASEURL=${BASEURL:-"http://localhost:7070/api/V2"}

get_id() {
    local type=$1
    local name=$2
    curl -s "${BASEURL}/${type}?name=${name}" | jq -r '.id // .[0].id // empty'
}

echo "Gathering IDs from dog-trainer..."

TEST_PROFILE_ID=$(get_id "profile" "test")
TEST_GROUP_ID=$(get_id "group" "test")
EPMD_SERVICE_ID=$(get_id "service" "test-epmd")
OFFICE_ZONE_ID=$(get_id "zone" "office")

DREW_PROFILE_ID=$(get_id "profile" "drewtest")
DREW_GROUP_ID=$(get_id "group" "drewtest")
DREW_ZONE_ID=$(get_id "zone" "drewtest")

LOCAL_PROFILE_ID=$(get_id "profile" "local_group")

echo "Updating ruleset_test.json..."
if [[ -n "$TEST_PROFILE_ID" && -n "$TEST_GROUP_ID" && -n "$EPMD_SERVICE_ID" && -n "$OFFICE_ZONE_ID" ]]; then
    jq --arg pid "$TEST_PROFILE_ID" \
       --arg gid "$TEST_GROUP_ID" \
       --arg zid "$OFFICE_ZONE_ID" \
       --arg svc "$EPMD_SERVICE_ID" \
       '.profile_id = $pid | 
        .rules.inbound[0].group = $gid | 
        .rules.inbound[0].service = $svc | 
        .rules.inbound[1].group = $zid | 
        .rules.inbound[1].service = $svc' \
       ruleset_test.json > ruleset_test.json.tmp && mv ruleset_test.json.tmp ruleset_test.json
    echo "ruleset_test.json updated."
else
    echo "Warning: Could not find all IDs for ruleset_test.json"
fi

echo "Updating ruleset_epmd.json..."
if [[ -n "$DREW_PROFILE_ID" && -n "$DREW_GROUP_ID" && -n "$DREW_ZONE_ID" && -n "$EPMD_SERVICE_ID" ]]; then
    jq --arg pid "$DREW_PROFILE_ID" \
       --arg gid "$DREW_GROUP_ID" \
       --arg zid "$DREW_ZONE_ID" \
       --arg svc "$EPMD_SERVICE_ID" \
       '.profile_id = $pid | 
        .rules.inbound[1].group = $gid | 
        .rules.inbound[1].service = $svc | 
        .rules.inbound[2].group = $zid | 
        .rules.inbound[2].service = $svc' \
       ruleset_epmd.json > ruleset_epmd.json.tmp && mv ruleset_epmd.json.tmp ruleset_epmd.json
    echo "ruleset_epmd.json updated."
else
    echo "Warning: Could not find all IDs for ruleset_epmd.json"
fi

echo "Updating ruleset_local_group.json..."
if [[ -n "$LOCAL_PROFILE_ID" ]]; then
    jq --arg pid "$LOCAL_PROFILE_ID" '.profile_id = $pid' ruleset_local_group.json > ruleset_local_group.json.tmp && mv ruleset_local_group.json.tmp ruleset_local_group.json
    echo "ruleset_local_group.json updated."
else
    echo "Warning: Could not find all IDs for ruleset_local_group.json"
fi

echo "Done."
