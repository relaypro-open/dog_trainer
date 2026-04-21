# dog_trainer E2E Test Harness

This directory contains end-to-end tests for the `dog_trainer` and `dog_agent` pipeline.

## Overview

The tests verify that:
1. REST API calls to `dog_trainer` correctly update the system state.
2. `dog_trainer` generates and publishes rules to RabbitMQ.
3. `dog_agent` consumes these rules and applies them via `iptables` and `ipset`.
4. The applied rules correctly reflect the intended network connectivity.

## Prerequisites

- `docker` and `docker compose`
- `jq`
- `envsubst` (from `gettext`)
- The `dog` stack orchestration file at `../docker-compose.local_deploy.yml`.

## Running the tests

From the project root:

```bash
./test/e2e/run.sh
```

### Options

- `KEEP_RUNNING=1`: Prevents the test harness from tearing down the Docker Compose stack after completion. Useful for debugging.
- `TF_VAR_dog_api_token_sandbox`: The API token used for requests. Defaults to `dummy-token`.

## Test Scenarios

### 1. Two-agent Reachability (`scenario_two_agent.sh`)
- Populates a zone with both agent IPs.
- Configures an SSH service.
- Creates two groups, one for each agent.
- Configures a profile for the primary group that allows SSH from the other group but denies HTTP.
- Verifies that `nc` succeeds for port 22 and fails for port 80.

### 2. Profile Change (`scenario_profile_change.sh`)
- Mutates the profile to deny SSH.
- Waits for hash convergence (meaning the agent has reported it applied the new rules).
- Verifies that `nc` now fails for port 22.

## Debugging

If a test fails, the harness will dump:
- The last 50 lines of `dog-trainer` logs.
- The `iptables-save` output from the agent.
- The `ipset list` output from the agent.

Use `KEEP_RUNNING=1 ./test/e2e/run.sh` to keep the environment alive and manually inspect state.
