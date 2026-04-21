#!/bin/bash
set -x
set -euo pipefail

# Scenario: Two-agent reachability
# 1. Setup zones, services, groups, profiles
# 2. Assign hosts to groups
# 3. Wait for convergence
# 4. Verify connectivity

source "$(dirname "$0")/e2e_lib.sh"

echo "Running scenario: Two-agent reachability"

# 0. Discover IPs
export DOG_AGENT_IP=$(get_agent_ip "dog-agent")
export DOG_AGENT_2_IP=$(get_agent_ip "dog-agent-2")
echo "dog-agent IP: $DOG_AGENT_IP"
echo "dog-agent-2 IP: $DOG_AGENT_2_IP"

# 1. Setup
echo "Creating Zone..."
export ZONE_ID=$(post <(envsubst < fixtures/zone_agent.json.template) "${BASEURL}/V2/zone")

echo "Creating Services..."
export SSH_SERVICE_ID=$(post fixtures/service_ssh.json "${BASEURL}/V2/service")
export HTTP_SERVICE_ID=$(post fixtures/service_http.json "${BASEURL}/V2/service")

echo "Creating Groups..."
export GROUP_PRIMARY_ID=$(post fixtures/group_primary.json "${BASEURL}/V2/group")
export GROUP_2_ID=$(post fixtures/group_2.json "${BASEURL}/V2/group")

echo "Creating Profiles..."
export PROFILE_PRIMARY_ID=$(post <(envsubst < fixtures/profile_primary.json.template) "${BASEURL}/V2/profile")
export PROFILE_2_ID=$(post <(envsubst < fixtures/profile_2.json.template) "${BASEURL}/V2/profile")

echo "Assigning Profiles to Groups..."
put <(envsubst < fixtures/group_primary_update.json.template) "${BASEURL}/V2/group/${GROUP_PRIMARY_ID}"
put <(envsubst < fixtures/group_2_update.json.template) "${BASEURL}/V2/group/${GROUP_2_ID}"

# 2. Assign Hosts to Groups
echo "Waiting for hosts to register..."
HOST_PRIMARY_ID=$(wait_for_host_registered "dog-agent" 60)
export HOST_PRIMARY_ID
HOST_2_ID=$(wait_for_host_registered "dog-agent-2" 60)
export HOST_2_ID

echo "Assigning Hosts to Groups..."
put <(envsubst < fixtures/host_primary_update.json.template) "${BASEURL}/V2/host/${HOST_PRIMARY_ID}"
put <(envsubst < fixtures/host_2_update.json.template) "${BASEURL}/V2/host/${HOST_2_ID}"

# 3. Wait for Convergence
echo "Waiting for convergence..."
wait_for_convergence "${GROUP_PRIMARY_ID}" "${HOST_PRIMARY_ID}" 60
wait_for_convergence "${GROUP_2_ID}" "${HOST_2_ID}" 60

# 4. Verify
echo "Verifying connectivity..."
# Give it 1s to settle as per plan
sleep 1

# From dog-agent-2 to dog-agent:
# SSH (22) should be OPEN
assert_connectivity "dog-agent-2" "$DOG_AGENT_IP" 22 "open"
# HTTP (80) should be CLOSED (denied by profile)
assert_connectivity "dog-agent-2" "$DOG_AGENT_IP" 80 "closed"

echo "Scenario 'Two-agent reachability' passed!"
