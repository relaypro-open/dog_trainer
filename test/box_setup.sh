#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'
#This script should be run on dog-vm-host
source test_lib.sh
TESTDIR=box
BASEURL=http://dog-server:7070/api

echo "box env setup"

echo "LOCAL IP"
export VM_IP=$(facter networking.interfaces.lxdbr0.ip)
echo $VM_IP

echo "ZONES"
export VM_HOST_ZONE_ID=$(post <(cat "${TESTDIR}/zone_vm_host.json.sh" | envsubst) "${BASEURL}/zone")
#get ${BASEURL}/zone/${VM_HOST_ZONE_ID}
#get ${BASEURL}/zone/${VM_HOST_ZONE_ID}X
export LOCALHOST_ZONE_ID=$(post "${TESTDIR}/zone_localhost.json" "${BASEURL}/zone")
#echo $VM_HOST_ZONE_ID
echo "SERVICES"
export HTTP_SERVICE_ID=$(post "${TESTDIR}/service_http.json" "${BASEURL}/service")
#echo $HTTP_SERVICE_ID
export SSH_SERVICE_ID=$(post "${TESTDIR}/service_ssh.json" "${BASEURL}/service")
#echo $SSH_SERVICE_ID
export POSTGRES_SERVICE_ID=$(post "${TESTDIR}/service_postgres.json" "${BASEURL}/service")

echo "Create GROUPS"
export DB_GROUP_ID=$(post "${TESTDIR}/group_db.json" "${BASEURL}/group")
#echo $DB_GROUP_ID
export APP_GROUP_ID=$(post "${TESTDIR}/group_app.json" "${BASEURL}/group")
#echo $APP_GROUP_ID

echo "PROFILES"
export DB_PROFILE_ID=$(post <(cat "${TESTDIR}/profile_db.json.sh" | envsubst) "${BASEURL}/profile")
export APP_PROFILE_ID=$(post <(cat "${TESTDIR}/profile_app.json.sh" | envsubst) "${BASEURL}/profile")

echo "Assign PROFILES to GROUPS"
#cat <(cat "${TESTDIR}/group_db_update.json.sh" | envsubst)
put <(cat "${TESTDIR}/group_db_update.json.sh" | envsubst) "${BASEURL}/group/${DB_GROUP_ID}"
#cat <(cat "${TESTDIR}/group_app_update.json.sh" | envsubst)
put <(cat "${TESTDIR}/group_app_update.json.sh" | envsubst) "${BASEURL}/group/${APP_GROUP_ID}"

test_report
