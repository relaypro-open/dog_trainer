#!/bin/bash
set -x
set -euo pipefail

# E2E Test Runner
# 1. Start Docker Compose stack
# 2. Wait for health
# 3. Run scenarios
# 4. Teardown (unless KEEP_RUNNING=1)

export KEEP_RUNNING=${KEEP_RUNNING:-0}
export TF_VAR_dog_api_token_sandbox=${TF_VAR_dog_api_token_sandbox:-"dummy-token"}

# Paths
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
COMPOSE_FILE="$(cd "$SCRIPT_DIR/../../.." && pwd)/docker-compose.local_deploy.yml"
PROJECT_NAME="dog_e2e"

# Ensure secrets file exists for compose
if [ ! -f "$SCRIPT_DIR/../../../compose_secrets.txt" ]; then
    echo "Creating dummy compose_secrets.txt"
    echo "dummy" > "$SCRIPT_DIR/../../../compose_secrets.txt"
fi

teardown() {
    if [ "$KEEP_RUNNING" -eq 1 ]; then
        echo "KEEP_RUNNING=1, skipping teardown."
    else
        echo "Tearing down..."
        docker-compose -p "$PROJECT_NAME" -f "$COMPOSE_FILE" down -v
    fi
}

trap teardown EXIT

echo "Starting Docker Compose stack ($PROJECT_NAME)..."
docker-compose -p "$PROJECT_NAME" -f "$COMPOSE_FILE" up -d

# Source library for helpers
export PASS=0
export FAIL=0
source "$SCRIPT_DIR/e2e_lib.sh"

# Wait for trainer to be healthy, restarting it if it crashes due to rabbitmq race
echo "Waiting for http://localhost:7070/api/healthcheck to be healthy..."
for i in {1..45}; do
    if curl --silent --fail "http://localhost:7070/api/healthcheck" > /dev/null; then
        echo "dog_trainer is healthy!"
        HEALTHY=1
        break
    fi
    #echo "Not healthy yet. Checking if dog-trainer is asleep..."
    #if docker logs dog-trainer 2>&1 | tail -n 10 | grep -q "sleep infinity"; then
    #    echo "dog_trainer crashed. Restarting it..."
    #    docker-compose -p "$PROJECT_NAME" -f "$COMPOSE_FILE" restart dog_trainer
    #elif docker logs dog-trainer 2>&1 | tail -n 10 | grep -q "Crash dump"; then
    #    echo "dog_trainer crashed. Restarting it..."
    #    docker-compose -p "$PROJECT_NAME" -f "$COMPOSE_FILE" restart dog_trainer
    #fi
    sleep 2
done

if [ -z "${HEALTHY:-}" ]; then
    echo "dog_trainer failed to become healthy."
    exit 1
fi

# Setup baseline resources (formerly Terraform)
cd "$SCRIPT_DIR"
./setup_baseline.sh

# Run scenarios
# Initialize pass/fail counters
echo 0 > /tmp/pass.txt
echo 0 > /tmp/fail.txt

echo "Running scenarios..."
# We need to be in the e2e directory so fixtures are found
cd "$SCRIPT_DIR"
./scenario_two_agent.sh

if [ -f "./scenario_profile_change.sh" ]; then
    ./scenario_profile_change.sh
fi

test_report
