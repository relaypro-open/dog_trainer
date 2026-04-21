#!/bin/bash
set -euo pipefail

# setup_baseline.sh
# Replaces the Terraform setup in control/terraform/dog/

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
source "$SCRIPT_DIR/e2e_lib.sh"

echo "Setting up baseline resources..."

echo "Creating Fact: docker..."
post "$SCRIPT_DIR/fixtures/baseline/fact_docker.json" "${BASEURL}/V2/fact" > /dev/null

echo "Creating Zone: test_zone..."
export ZONE_TEST_ZONE_ID=$(post "$SCRIPT_DIR/fixtures/baseline/zone_test_zone.json" "${BASEURL}/V2/zone")

echo "Creating Service: ssh-tcp-22..."
export SERVICE_SSH_ID=$(post "$SCRIPT_DIR/fixtures/baseline/service_ssh_tcp_22.json" "${BASEURL}/V2/service")

echo "Creating Profile: test_drew..."
export PROFILE_TEST_DREW_ID=$(post <(envsubst < "$SCRIPT_DIR/fixtures/baseline/profile_test_drew.json.template") "${BASEURL}/V2/profile")

echo "Creating Profile: test_qa..."
export PROFILE_TEST_QA_ID=$(post <(envsubst < "$SCRIPT_DIR/fixtures/baseline/profile_test_qa.json.template") "${BASEURL}/V2/profile")

echo "Creating Group: test_qa..."
post <(envsubst < "$SCRIPT_DIR/fixtures/baseline/group_test_qa.json.template") "${BASEURL}/V2/group" > /dev/null

echo "Creating Group: test_group..."
post <(envsubst < "$SCRIPT_DIR/fixtures/baseline/group_test_group.json.template") "${BASEURL}/V2/group" > /dev/null

echo "Creating Group: local_group..."
post <(envsubst < "$SCRIPT_DIR/fixtures/baseline/group_local_group.json.template") "${BASEURL}/V2/group" > /dev/null

echo "Baseline setup complete."
