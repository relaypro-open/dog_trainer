#!/bin/bash

#base_url=http://localhost:7070/api/V2
#base_url=http://localhost:7071/api/V2
#base_url=https://dog-qa.relaydev.sh:8443/api/V2
base_url=http://dog-qa.relaydev.sh:7070/api/V2

hosts=$(curl -s -H "Authorization: Bearer $TF_VAR_dog_api_token_qa" -H 'Content-Type: application/json' "$base_url/hosts?active=true" | jq -r .[].hostkey)
echo $hosts

while true; do
    date
    while IFS= read -r hostkey; do
      # Process each line here
      #echo "Processing host: $hostkey"
        #curl -X POST $base_url/file_transfer/$hostkey \
        curl -H "Authorization: Bearer $TF_VAR_dog_api_token_qa" -X POST $base_url/file_transfer/$hostkey \
            -F 'data=@/tmp/hosts;type=text/csv;filename=/tmp/hosts2' \
            -s -o /dev/null -w "%{url_effective} %{http_code}\n" | grep -v " 200" &
    done < <(echo "$hosts")
    sleep 10
done
