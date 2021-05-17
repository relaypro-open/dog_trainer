#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

BASEURL=http://localhost:7070/api
#TESTDIR=/home/dgulino/dog_trainer/test
TESTDIR=/opt/dog_trainer/test

PASS=0
FAIL=0

function log() {
  echo "${BASH_LINENO[-2]}: $@"
}

post() {
  DATA=$1
  URL=$2
  R=$(curl -d @"${DATA}" -w "\n%{http_code}\n" --silent --show-error  -H "Content-Type: application/json" -X POST "${URL}")
  BODY=$(echo ${R} | awk '{print $1}')
  RESPONSE_CODE=$(echo ${R} | awk '{print $2}')
  if [ "$RESPONSE_CODE" != "500" ]
  then
    let PASS=${PASS}+1
    >&2 log "pass: POST ${URL}"
    #echo ${BODY}
    #echo ${RESPONSE_CODE}
    ID=$(echo ${BODY} | jq -r .id)
    echo ${ID}
  else
    let FAIL=${FAIL}+1
    >&2 log "fail: ${R} != ${RESPONSE_CODE}, POST ${URL}"
  fi
}

c_data() {
  METHOD=$1
  DATA=$2
  URL=$3
  CODE=$4
  R=$(curl -d @"${DATA}" -H "Content-Type: application/json" -X ${METHOD} --write-out %{http_code} --silent --output /dev/null --show-error ${URL})
    
  if [ "$R" == "$CODE" ];then let PASS=${PASS}+1;log "pass: ${METHOD} ${URL}";else let FAIL=${FAIL}+1;log "fail: ${R} != ${CODE}, ${METHOD} ${URL}";fi
}

putc() {
  DATA=$1
  URL=$2
  CODE=$3
  c_data PUT ${DATA} ${URL} ${CODE}
}

put() {
  DATA=$1
  URL=$2
  putc ${DATA} ${URL} 303
}

c_nodata() {
  METHOD=$1
  URL=$2
  CODE=$3
  R=$(curl --write-out %{http_code} --silent --output /dev/null --show-error -H "Content-Type: application/json" -X ${METHOD} ${URL})
  if [ "$R" == "$CODE" ];then let PASS=${PASS}+1;log "pass: ${METHOD} ${URL}";else let FAIL=${FAIL}+1;log "fail: ${R} != ${CODE}, ${METHOD} ${URL}";fi
}

getc() {
  URL=$1
  CODE=$2
  c_nodata GET ${URL} ${CODE}
}

get() {
  URL=$1
  getc ${URL} 200
}

delete() {
  URL=$1
  c_nodata DELETE ${URL} 204
}

get_id() {
  URL=$1
  R=$(curl -w "\n%{http_code}\n" --silent --show-error  -H "Content-Type: application/json" -X GET "${URL}")
  BODY=$(echo ${R} | awk '{print $1}')
  RESPONSE_CODE=$(echo ${R} | awk '{print $2}')
  if [ "$RESPONSE_CODE" != "500" ]
  then
    let PASS=${PASS}+1
    >&2 log "pass: POST ${URL}"
    #echo ${BODY}
    #echo ${RESPONSE_CODE}
    ID=$(echo ${BODY} | jq -r .id)
    echo ${ID}
  else
    let FAIL=${FAIL}+1
    >&2 log "fail: ${R} != ${RESPONSE_CODE}, GET ${URL}"
  fi
}

echo "dog_trainer basic API test"
#echo -e "{\n\"name\":\"office\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"2.2.2.2\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone.json
#ZONE_ID=$(curl -d "${TESTDIR}/zone.json' --silent -H "Content-Type: application/json" -X POST "${BASEURL}/zone" | jq -r .id)
echo "ZONE"
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"2.2.2.2\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone.json
ZONE_ID=$(post "${TESTDIR}/zone.json" "${BASEURL}/zone")
#echo "ZONE_ID: ${ZONE_ID}"
get ${BASEURL}/zone/${ZONE_ID}
get ${BASEURL}/zone?name=drewtest
echo -e "{\n\"name\":\"drewtest\",\n\"ipv4_addresses\":[\"1.1.1.1\",\"3.3.3.3\"],\n\"ipv6_addresses\":[]\n}" > ${TESTDIR}/zone_update.json
put "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}"
putc "${TESTDIR}/zone_update.json" "${BASEURL}/zone/${ZONE_ID}" 303
get ${BASEURL}/zone/${ZONE_ID}
delete ${BASEURL}/zone/${ZONE_ID}
get ${BASEURL}/zones

echo "SERVICE"
SERVICE_ID=$(post "${TESTDIR}/service.json" "${BASEURL}/service")
get ${BASEURL}/service/${SERVICE_ID} 
get ${BASEURL}/service?name=drewtest
echo -e "{\n\"name\":\"drewtest2\"\n}" > ${TESTDIR}/service_update.json
put "${TESTDIR}/service_update.json" "${BASEURL}/service/${SERVICE_ID}"
putc "${TESTDIR}/service_update.json" "${BASEURL}/service/${SERVICE_ID}" 303
putc "${TESTDIR}/service_update_no_ports.json" "${BASEURL}/service/${SERVICE_ID}" 500
putc "${TESTDIR}/service_update_string_ports.json" "${BASEURL}/service/${SERVICE_ID}" 500
get ${BASEURL}/service/${SERVICE_ID}
delete ${BASEURL}/service/${SERVICE_ID}
get ${BASEURL}/services

#ssh service
#SERVICE_ID=$(curl -d "${TESTDIR}/service_any.json" -H "Content-Type: application/json" -X POST "${BASEURL}/service" | jq -r .id)
SERVICE_ID=$(post "${TESTDIR}/service_ssh.json" "${BASEURL}/service")

echo "PROFILE"
PROFILE_ID=$(post "${TESTDIR}/profile.json" "${BASEURL}/profile")
get ${BASEURL}/profile/${PROFILE_ID}
get ${BASEURL}/profile?name=drewtest
put "${TESTDIR}/profile_update.json" "${BASEURL}/profile/${PROFILE_ID}"
putc "${TESTDIR}/profile_update.json" "${BASEURL}/profile/${PROFILE_ID}" 303 #updates create new profiles
putc "${TESTDIR}/profile_update_blank_name.json" "${BASEURL}/profile/${PROFILE_ID}" 500
get ${BASEURL}/profile/${PROFILE_ID}
#delete ${BASEURL}/profile/${PROFILE_ID}
get ${BASEURL}/profiles

echo "GROUP"
GROUP_ID=$(post "${TESTDIR}/group.json" "${BASEURL}/group")
get ${BASEURL}/group/${GROUP_ID}
get "${BASEURL}/group?name=drewtest"
put "${TESTDIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}"
putc "${TESTDIR}/group_update.json" "${BASEURL}/group/${GROUP_ID}" 303
putc "${TESTDIR}/group_update_blank_name.json" "${BASEURL}/group/${GROUP_ID}" 500
get ${BASEURL}/group/${GROUP_ID}
delete ${BASEURL}/group/${GROUP_ID}
get ${BASEURL}/groups

echo "HOST"
HOST_ID=$(post "${TESTDIR}/host.json" "${BASEURL}/host")
get ${BASEURL}/host/${HOST_ID}
get "${BASEURL}/host?name=drewtest.phoneboothdev.info"
put "${TESTDIR}/host_update.json" "${BASEURL}/host/${HOST_ID}"
putc "${TESTDIR}/host_update.json" "${BASEURL}/host/${HOST_ID}" 303
putc "${TESTDIR}/host_update_blank_hostname.json" "${BASEURL}/host/${HOST_ID}" 500
get ${BASEURL}/host/${HOST_ID}
delete ${BASEURL}/host/${HOST_ID}
get ${BASEURL}/hosts

echo "LINK/EXTERNAL"
HOST_ID=$(post "${TESTDIR}/link.json" "${BASEURL}/link")
get ${BASEURL}/link/${HOST_ID}
get "${BASEURL}/link?name=x1"
EXTERNAL_ID=$(get_id "${BASEURL}/external?name=x1")
put "${TESTDIR}/link_update.json" "${BASEURL}/link/${HOST_ID}"
get ${BASEURL}/link/${HOST_ID}
delete ${BASEURL}/link/${HOST_ID}
getc "${BASEURL}/link?name=x1" 404
getc "${BASEURL}/external?name=x1" 404
get ${BASEURL}/links

echo
echo "PASS: ${PASS} / FAIL: ${FAIL}"
