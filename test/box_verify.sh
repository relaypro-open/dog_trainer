#!/bin/bash
PASS=0
FAIL=0

SSH_PARAMETERS="-q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

c() {
	CMD=$1
	EXPECTED=$2
	RESULT=$($CMD 2>&1)
	if [[ "$RESULT" == "$EXPECTED" ]]
	then 
		((PASS=$PASS+1))
		echo "PASS: $CMD == \"$RESULT\""
	else 
		((FAIL=$FAIL+1)) 
		echo "FAIL: $CMD | \"$RESULT\" != \"$EXPECTED\""
	fi
}

report() {
	echo
	echo "PASS: $PASS / FAIL: $FAIL"
	if [[ $FAIL -gt 0 || $PASS -lt 1 ]]
	then
		exit 1
	else
		exit 0
	fi
}

echo "Testing dog_in_a_box firewall rules:"
echo
c "nc -vz -w 1 app-server http" "Connection to app-server 80 port [tcp/http] succeeded!"

c "nc -vz -w 1 db-server postgres" "nc: connect to db-server port 5432 (tcp) timed out: Operation now in progress"

c "ssh ${SSH_PARAMETERS} root@app-server bash -c 'nc -vz -w 1 db-server postgres'" "Connection to db-server 5432 port [tcp/postgresql] succeeded!"

c "ssh ${SSH_PARAMETERS} root@db-server bash -c 'nc -vz -w 1 app-server http'" "Connection to app-server 80 port [tcp/http] succeeded!"
report
