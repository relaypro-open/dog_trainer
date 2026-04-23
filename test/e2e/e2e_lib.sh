#!/bin/bash -x
set -euo pipefail

# Source existing test library for post/put/get helpers
source "$(dirname "$0")/../test_lib.sh"

# Override or set BASEURL for E2E
export BASEURL=${BASEURL:-"http://localhost:7070/api"}

wait_for_host_registered() {
    local hostname=$1
    local timeout=$2
    local start_time=$(date +%s)
    echo "Waiting for host $hostname to register..." >&2
    while true; do
        local host_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/host?name=${hostname}")
        local host_id=$(echo "$host_json" | jq -r 'if type=="array" then .[0].id else .id end')
        if [[ -n "$host_id" && "$host_id" != "null" && "$host_id" != "" ]]; then
            echo "Host $hostname registered with ID $host_id" >&2
            echo "$host_id"
            return 0
        fi
        local current_time=$(date +%s)
        if (( current_time - start_time > timeout )); then
            echo "Timeout waiting for host $hostname to register" >&2
            return 1
        fi
        sleep 10
    done
}

wait_for_convergence() {
    local group_id=$1
    local host_id=$2
    local timeout=$3
    local start_time=$(date +%s)
    echo "Waiting for convergence between group $group_id and host $host_id..."
    while true; do
        if [[ -z "$host_id" || "$host_id" == "null" ]]; then
            echo "Error: host_id is empty or null in wait_for_convergence." >&2
            return 1
        fi

        # Use curl directly to get JSON bodies by ID
        local group_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/group/${group_id}")
        local host_json=$(curl ${OPTS[@]} --silent -H "Authorization: Bearer $TF_VAR_dog_api_token_sandbox" "${BASEURL}/host/${host_id}")

        local group_ipt_hash=$(echo "$group_json" | jq -r '.hash4_iptables // empty')
        local group_ips_hash=$(echo "$group_json" | jq -r '.hash4_ipsets // empty')
        local host_ipt_hash=$(echo "$host_json" | jq -r '.hash4_iptables // empty')
        local host_ips_hash=$(echo "$host_json" | jq -r '.hash4_ipsets // empty')

        if [ -n "$group_ipt_hash" ] && [ "$group_ipt_hash" = "$host_ipt_hash" ] && [ -n "$group_ips_hash" ] && [ "$group_ips_hash" = "$host_ips_hash" ]; then
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
            agent_exec "dog-agent-1" iptables-save || true
            echo "--- Host IPset List ---"
            agent_exec "dog-agent-1" ipset list || true
            echo "--- Trainer Logs Tail ---"
            docker-compose -p dog_e2e logs --tail 50 dog-trainer || true
            return 1
        fi
        sleep 10
    done
}

agent_exec() {
    local container=$1
    shift
    docker exec "$container" "$@"
}

get_agent_ip() {
    local container=$1
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$container"
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

resolve_json_template() {
    local template=$1
    local output=$2
    shift 2
    
    local jq_args=()
    local jq_expr="walk(if type == \"string\" then . as \$val | "
    
    local first=1
    while (( "$#" )); do
        local key=$1
        local val=$2
        shift 2
        
        # Unique arg name for jq
        local arg_name="v$(echo -n "$key" | md5sum | cut -c1-8)"
        jq_args+=("--arg" "$arg_name" "$val")
        
        # Determine if we should parse as boolean
        local parse_val="\$$arg_name"
        if [ "$val" = "true" ]; then
             parse_val="true"
        elif [ "$val" = "false" ]; then
             parse_val="false"
        fi
        
        if [ $first -eq 1 ]; then
            jq_expr="${jq_expr} if \$val == \"${key}\" then ${parse_val}"
            first=0
        else
            jq_expr="${jq_expr} elif \$val == \"${key}\" then ${parse_val}"
        fi
    done
    
    if [ $first -eq 0 ]; then
        jq_expr="${jq_expr} else \$val end else . end)"
    else
        jq_expr="."
    fi
    
    jq "${jq_args[@]}" "$jq_expr" "$template" > "$output"
}
