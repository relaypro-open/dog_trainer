#!/bin/bash
USER=$1
PASSWORD=$2

rabbitmqctl add_user ${USERNAME} ${PASSWORD} 
rabbitmqctl change_password ${USERNAME} ${PASSWORD}
rabbitmqctl set_user_tags ${USERNAME} administrator
rabbitmqctl set_permissions ${USERNAME} '.*' '.*' '.*'

rabbitmqctl add_user --vhost dog ${USERNAME} ${PASSWORD} 
rabbitmqctl change_password --vhost dog ${USERNAME} ${PASSWORD}
rabbitmqctl set_user_tags ${USERNAME} --vhost dog administrator
rabbitmqctl set_permissions ${USERNAME} --vhost dog '.*' '.*' '.*'
