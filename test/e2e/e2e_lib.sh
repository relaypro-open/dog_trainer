#!/bin/bash -x
set -euo pipefail

# Source existing test library for post/put/get helpers
# Assuming we are in test/e2e/
source "$(dirname "$0")/../test_lib.sh"

# Override or set BASEURL for E2E
export BASEURL=${BASEURL:-"http://localhost:7070/api"}

wait_for_healthy() {
    local url=$1
    local timeout=$2
    local start_time=$(date +%s)
    echo "Waiting for $url to be healthy..."
    while true; do
        if curl --silent --fail "$url" > /dev/null; then
            echo "URL $url is healthy."
            return 0
        fi
        local current_time=$(date +%s)
        if (( current_time - start_time > timeout )); then
            echo "Timeout waiting for $url"
            return 1
        fi
        sleep 2
    done
}

wait_for_host_registered() {
    local hostname=$1
    local timeout=$2
    local start_time=$(date +%s)
    echo "Waiting for host $hostname to register..." >&2
    while true; do
        local host_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/V2/host?name=${hostname}")
        # If the response is a list and not empty
        if [[ $(echo "$host_json" | jq '. | length') -gt 0 ]]; then
            local host_id=$(echo "$host_json" | jq -r '.[0].id')
            if [[ "$host_id" != "null" ]]; then
                echo "Host $hostname registered with ID $host_id" >&2
                echo "$host_id"
                return 0
            fi
        fi
        local current_time=$(date +%s)
        if (( current_time - start_time > timeout )); then
            echo "Timeout waiting for host $hostname to register" >&2
            return 1
        fi
        sleep 2
    done
}

wait_for_convergence() {
    local group_id=$1
    local host_id=$2
    local timeout=$3
    local start_time=$(date +%s)
    echo "Waiting for convergence between group $group_id and host $host_id..."
    while true; do
        local group_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/V2/group/${group_id}")
        local host_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/V2/host/${host_id}")

        local group_ipt_hash=$(echo "$group_json" | jq -r '.hash4_iptables // empty')
        local group_ips_hash=$(echo "$group_json" | jq -r '.hash4_ipsets // empty')
        local host_ipt_hash=$(echo "$host_json" | jq -r '.hash4_iptables // empty')
        local host_ips_hash=$(echo "$host_json" | jq -r '.hash4_ipsets // empty')

        if [[ -n "$group_ipt_hash" && "$group_ipt_hash" == "$host_ipt_hash" && -n "$group_ips_hash" && "$group_ips_hash" == "$host_ips_hash" ]]; then
            echo "Convergence reached!"
            return 0
        fi

        local current_time=$(date +%s)
        if (( current_time - start_time > timeout )); then
            echo "Timeout waiting for convergence"
            echo "Group ($group_id) hashes: ipt=$group_ipt_hash, ips=$group_ips_hash"
            echo "Host  ($host_id) hashes: ipt=$host_ipt_hash, ips=$host_ips_hash"
            # Failure diagnostics
            echo "--- Host IPtables Save ---"
            agent_exec "dog-agent" "iptables-save" || true
            echo "--- Host IPset List ---"
            agent_exec "dog-agent" "ipset list" || true
            echo "--- Trainer Logs Tail ---"
            docker-compose -p dog_e2e logs --tail 50 dog-trainer || true
            return 1
        fi
        sleep 2
    done
}

agent_exec() {
    local container=$1
    shift
    docker exec "$container" "$@"
}

get_agent_ip() {
    local container=$1
    # Assuming project name 'dog_e2e' as per run.sh
    docker inspect -f '{{.NetworkSettings.Networks.dog_e2e_default.IPAddress}}' "$container"
}

assert_connectivity() {
    local from=$1
    local to_ip=$2
    local port=$3
    local expected=$4 # open|closed

    echo "Checking connectivity from $from to $to_ip:$port (expected: $expected)..."
    if agent_exec "$from" nc -zv -w2 "$to_ip" "$port" > /dev/null 2>&1; then
        if [[ "$expected" == "open" ]]; then
            echo "PASS: $to_ip:$port is open as expected"
        else
            echo "FAIL: $to_ip:$port is open but expected closed"
            return 1
        fi
    else
        if [[ "$expected" == "closed" ]]; then
            echo "PASS: $to_ip:$port is closed as expected"
        else
            echo "FAIL: $to_ip:$port is closed but expected open"
            return 1
        fi
    fi
}

assert_iptables_contains() {
    local container=$1
    local pattern=$2
    echo "Checking if iptables on $container contains '$pattern'..."
    if agent_exec "$container" iptables-save | grep -q "$pattern"; then
        echo "PASS: iptables contains '$pattern'"
    else
        echo "FAIL: iptables does not contain '$pattern'"
        return 1
    fi
}
