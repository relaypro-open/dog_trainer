#!/bin/bash
passkey=$(curl -s http://csc:8000/csc/register | jq -r .passkey)
certs=$(curl -s -d '{"fqdn": "rabbitmq", "passkey": "'$passkey'"}' http://csc:8000/csc/cert)
echo $certs | jq -r .server_key > /etc/dog_trainer/private/server.key
echo $certs | jq -r .server_crt > /etc/dog_trainer/certs/server.crt
echo $certs | jq -r .ca_crt >     /etc/dog_trainer/certs/ca.crt
