#!/bin/bash
set -x
set -euo pipefail

# Scenario: Single-agent policy application
# 1. Setup zones, services, groups, profiles
# 2. Assign host to group
# 3. Wait for convergence
# 4. Verify iptables

source "$(dirname "$0")/e2e_lib.sh"

echo "Running scenario: Single-agent policy application"

# 0. Discover IPs
export DOG_AGENT_IP=$(get_agent_ip "dog-agent")
echo "dog-agent IP: $DOG_AGENT_IP"

# 1. Setup
echo "Creating Zone..."
export ZONE_ID=$(post <(echo '{
  "name": "agent_zone",
  "ipv4_addresses": ["'${DOG_AGENT_IP}'"],
  "ipv6_addresses": []
}') "${BASEURL}/V2/zone")

echo "Creating Services..."
export SSH_SERVICE_ID=$(post fixtures/service_ssh.json "${BASEURL}/V2/service")
export HTTP_SERVICE_ID=$(post fixtures/service_http.json "${BASEURL}/V2/service")

echo "Creating Group..."
export GROUP_PRIMARY_ID=$(post fixtures/group_primary.json "${BASEURL}/V2/group")

echo "Creating Profile..."
# We will just allow SSH from ANY and drop HTTP
export PROFILE_PRIMARY_ID=$(post <(echo '{
  "name": "p_primary",
  "rules": {
    "inbound": [
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "allow ssh",
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 1,
        "service": "ssh",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "DROP",
        "active": true,
        "comment": "deny http",
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 2,
        "service": "http",
        "states": [],
        "type": "BASIC"
      }
    ],
    "outbound": []
  },
  "version": "1.1"
}') "${BASEURL}/V2/profile")

echo "Assigning Profile to Group..."
put <(envsubst < fixtures/group_primary_update.json.template) "${BASEURL}/V2/group/${GROUP_PRIMARY_ID}"

# 2. Assign Host to Group
echo "Waiting for host to register..."
export HOST_PRIMARY_ID=$(wait_for_host_registered "dog-agent" 60)

echo "Assigning Host to Group..."
put <(envsubst < fixtures/host_primary_update.json.template) "${BASEURL}/V2/host/${HOST_PRIMARY_ID}"

# 3. Wait for Convergence
echo "Waiting for convergence..."
wait_for_convergence "${GROUP_PRIMARY_ID}" "${HOST_PRIMARY_ID}" 60

# 4. Verify
echo "Verifying rules applied..."
sleep 1

assert_iptables_contains "dog-agent" "allow ssh"
assert_iptables_contains "dog-agent" "deny http"

echo "Scenario 'Single-agent' passed!"
