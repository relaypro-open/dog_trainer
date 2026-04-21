#!/bin/bash -x
set -euo pipefail

# Scenario: Profile change (Reactive)
# 1. Update p_primary to deny SSH
# 2. Update group g_primary to use new profile ID
# 3. Wait for convergence
# 4. Verify connectivity (SSH should now be closed)

source "$(dirname "$0")/e2e_lib.sh"

echo "Running scenario: Profile change (Reactive)"

GROUP_PRIMARY_ID=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/V2/group?name=g_primary" | jq -r '.[0].id')
HOST_PRIMARY_ID=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/V2/host?name=dog-agent" | jq -r '.[0].id')
DOG_AGENT_IP=$(get_agent_ip "dog-agent")

echo "Updating Profile to deny SSH..."
NEW_PROFILE_ID=$(post fixtures/profile_primary_deny_ssh.json.template "${BASEURL}/V2/profile")
echo "New Profile ID: $NEW_PROFILE_ID"

echo "Updating Group to use new Profile..."
export PROFILE_PRIMARY_ID=$NEW_PROFILE_ID
put <(envsubst < fixtures/group_primary_update.json.template) "${BASEURL}/V2/group/${GROUP_PRIMARY_ID}"

echo "Waiting for convergence..."
wait_for_convergence "${GROUP_PRIMARY_ID}" "${HOST_PRIMARY_ID}" 60

echo "Verifying connectivity..."
sleep 1
assert_connectivity "dog-agent-2" "$DOG_AGENT_IP" 22 "closed"

echo "Scenario 'Profile change' passed!"
