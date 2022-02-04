#!/bin/bash
#set -x
set -euo pipefail
IFS=$'\n\t'

echo $PASS > /tmp/pass.txt
echo $FAIL > /tmp/fail.txt
BASEURL=http://localhost:7070/api
OPTS=""
#BASEURL=https://dog-ubuntu-server.lxd:8443/api
#OPTS=( --key /opt/dog_trainer/client.key.pem --cert /opt/dog_trainer/client.cert.pem --cacert /tmp/consul/ca/consul-root.cer )

function log() {
  echo "${BASH_LINENO[-2]}: $@"
}

passplus() {
	P=$(cat /tmp/pass.txt)
	((P=P+1))
	echo $P > /tmp/pass.txt
}

failplus() {
	F=$(cat /tmp/fail.txt)
	((F=F+1))
	echo $F > /tmp/fail.txt
}

getpass() {
	cat /tmp/pass.txt
}

getfail() {
	cat /tmp/fail.txt
}

post() {
  DATA=$1
  URL=$2
  R=$(curl ${OPTS} -d @"${DATA}" -w "|%{http_code}\n" --silent --show-error  -H "Content-Type: application/json" -X POST "${URL}")
  #RCMD="curl ${OPTS[@]} -d @"${DATA}" -w \"|%{http_code}\n\" --silent --show-error  -H \"Content-Type: application/json\" -X POST \"${URL}\""
  #>&2 log "RCMD: ${RCMD}"
  #R=$(${RCMD})
  >&2 log "R: ${R}"
  BODY=$(echo ${R} | awk -F"|" '{print $1}')
  RESPONSE_CODE=$(echo ${R} | awk -F"|" '{print $2}')
  #>&2 log ${RESPONSE_CODE}
  if [[ ${RESPONSE_CODE} -gt 400 ]] && [[ ${RESPONSE_CODE} -lt 600 ]] 
  then
    >&2 log "fail: CODE ${RESPONSE_CODE}, POST ${URL}"
    #>&2 log ${BODY}
    #>&2 log ${RESPONSE_CODE}
    failplus
  else
    >&2 log "pass: POST ${URL}"
    ID=$(echo ${BODY} | jq -r .id)
    echo ${ID}
    passplus
  fi
}

c_data() {
  METHOD=$1
  DATA=$2
  URL=$3
  CODE=$4
  R=$(curl ${OPTS[@]} -d @"${DATA}" -H "Content-Type: application/json" -X ${METHOD} --write-out %{http_code} --silent --output /dev/null --show-error ${URL})
  if [ "$R" == "$CODE" ];then passplus ;log "pass: ${METHOD} ${URL}";else failplus ;log "fail: ${R} != ${CODE}, ${METHOD} ${URL}";fi
  #if [ "$R" == "$CODE" ];then PASS=$((PASS+1));log "pass: ${METHOD} ${URL}";else let FAIL=${FAIL}+1;log "fail: ${R} != ${CODE}, ${METHOD} ${URL}";fi
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
  R=$(curl ${OPTS[@]} --write-out %{http_code} --silent --output /dev/null --show-error -H "Content-Type: application/json" -X ${METHOD} ${URL})
  if [ "$R" == "$CODE" ];then passplus ;log "pass: ${METHOD} ${URL}";else failplus ;log "fail: ${R} != ${CODE}, ${METHOD} ${URL}";fi
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
  R=$(curl ${OPTS[@]} -w "\n%{http_code}\n" --silent --show-error  -H "Content-Type: application/json" -X GET "${URL}")
  BODY=$(echo ${R} | awk '{print $1}')
  RESPONSE_CODE=$(echo ${R} | awk '{print $2}')
  if [ "$RESPONSE_CODE" != "500" ]
  then
    passplus
    #let PASS=${PASS}+1
    >&2 log "pass: POST ${URL}"
    #echo ${BODY}
    #echo ${RESPONSE_CODE}
    ID=$(echo ${BODY} | jq -r .id)
    echo ${ID}
  else
    failplus
    #let FAIL=${FAIL}+1
    >&2 log "fail: ${R} != ${RESPONSE_CODE}, GET ${URL}"
  fi
}

test_report() {
echo
	
PASS=$(cat /tmp/pass.txt)
FAIL=$(cat /tmp/fail.txt)
echo "PASS: ${PASS} / FAIL: ${FAIL}"
if [[ "$FAIL" -gt 0 || "$PASS" -lt 1 ]]
then
	exit 1
else
	exit 0
fi

}
