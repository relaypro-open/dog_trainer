#!/bin/bash
echo "CERT_SOURCE: ${CERT_SOURCE}"

if [ "$CERT_SOURCE" == "csc" ]; then 
    /bin/bash -c "/etc/rabbitmq/csc_certs.sh" && /bin/bash -c "/opt/dog_trainer/bin/dog_trainer foreground"
else
    /bin/bash -c "/opt/dog_trainer/bin/dog_trainer foreground"
fi
