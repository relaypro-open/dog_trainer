#!/bin/bash

mkdir -p /etc/dog_trainer/private/
mkdir -p /etc/dog_trainer/certs/

HTTPD=`curl -A "Web Check" -sL --connect-timeout 3 -m 5 -w "%{http_code}\n" "http://csc:9000/csc/register" -o /dev/null`
until [ "$HTTPD" == "200" ]; do
    printf '.'
    sleep 3
    HTTPD=`curl -A "Web Check" -sL --connect-timeout 3 -m 5 -w "%{http_code}\n" "http://csc:9000/csc/register" -o /dev/null`
done

passkey=$(curl -s http://csc:9000/csc/register | jq -r .passkey)
certs=$(curl -s -d '{"fqdn": "rabbitmq", "passkey": "'$passkey'"}' -H "Content-Type: application/json" http://csc:9000/csc/cert)
echo $certs | jq -r .server_key > /etc/dog_trainer/private/server.key
echo $certs | jq -r .server_crt > /etc/dog_trainer/certs/server.crt
echo $certs | jq -r .ca_crt >     /etc/dog_trainer/certs/ca.crt
