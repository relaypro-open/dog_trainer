{
  "created": 1525360293813651,
  "name": "app",
  "rules": {
    "inbound": [
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "local",
        "environments": [],
        "group": "${LOCALHOST_ZONE_ID}",
        "group_type": "ZONE",
        "interface": "lo",
        "log": false,
        "log_prefix": "",
        "order": 1,
        "service": "any",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "test ssh",
        "environments": [],
        "group": "${DB_GROUP_ID}",
        "group_type": "ROLE",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 2,
        "service": "${SSH_SERVICE_ID}",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "http",
        "environments": [],
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 3,
        "service": "${HTTP_SERVICE_ID}",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "vm_host ssh",
        "environments": [],
        "group": "${VM_HOST_ZONE_ID}",
        "group_type": "ZONE",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 3,
        "service": "${SSH_SERVICE_ID}",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "DROP",
        "active": true,
        "comment": "",
        "environments": [],
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 5,
        "service": "any",
        "states": [],
        "type": "BASIC"
      }
    ],
    "outbound": [
      {
        "action": "ACCEPT",
        "active": false,
        "comment": "",
        "environments": [],
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 1,
        "service": "${SSH_SERVICE_ID}",
        "states": [],
        "type": "BASIC"
      },
      {
        "action": "ACCEPT",
        "active": true,
        "comment": "",
        "environments": [],
        "group": "any",
        "group_type": "ANY",
        "interface": "",
        "log": false,
        "log_prefix": "",
        "order": 2,
        "service": "any",
        "states": [],
        "type": "BASIC"
      }
    ]
  },
  "version": "1.1"
}
