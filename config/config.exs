import Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project: this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# third-party users: it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :dog: key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:dog: :key)
#
# You can also configure a third-party app:
#
#     config :logger: level: :info
#

# It is also possible to import configuration files: relative to this
# directory. For example: you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs: test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#Mix.env().exs"
#
#config :kernel,
#    logger_level: :notice,
#    logger: [
#                {:handler, :shell_log, :logger_std_h, %{
#                     :level => :all
#                }},
#       ]

config :dog_trainer, 
        env: "d1",
        aws_key: "",
        aws_secret: "",
        version: "20180423-180410-relflow",
        smtp_relay: "smtp.sendgrid.net",
        smtp_username: "apikey",
        smtp_password: "***REMOVED***",
        smtp_to: ["dgulino@relaypro.com"],
        smtp_from: "dog_trainer-dev@relaydev.sh",
        polling_interval_seconds: 60,
        keepalive_alert_seconds: 1800,
        hashcheck_alert_seconds: 180,
        rethinkdb_host: 'localhost',
        rethinkdb_port: 28015,
        rethinkdb_username: 'admin',
        rethinkdb_password: '',
        rethink_timeout_ms: 10000,
        rethink_squash_sec: 1.0,
        profile_periodic_publish_interval_seconds: 5,
        ipset_periodic_publish_interval_seconds: 5,
        check_v6_hashes: true,
        generate_unset_tables: false,
        max_interval_since_last_agent_update: 2,
        pools: [
           {:pool1, [
               {:size, 10},
               {:max_overflow, 20}
           ], []}
        ]

# Stop lager redirecting :error_logger messages
config :lager, :error_logger_redirect, false

# Stop lager removing Logger's :error_logger handler
config :lager, :error_logger_whitelist, [Logger.ErrorHandler]

# Stop lager writing a crash log
config :lager, :crash_log, false

config :lager,
  log_root: '/var/log/dog',
  handlers: [
    lager_console_backend: :info,
    lager_file_backend: [file: "error.log", level: :error],
    lager_file_backend: [file: "console.log", level: :info]
  ]

config :turtle,
        connection_config: [
            %{
                :conn_name => :default,
                :username => 'dog_trainer',
                :password => 'dog_trainer1',
                :virtual_host => 'dog',
                :ssl_options => [
                               {:cacertfile, '/var/consul/data/pki/certs/ca.crt'},
                               {:certfile, '/var/consul/data/pki/certs/server.crt'},
                               {:keyfile, '/var/consul/data/pki/private/server.key'},
                               {:verify, :verify_peer},
                               {:server_name_indication, :disable},
                               {:fail_if_no_peer_cert, true}
                              ],
                :deadline => 300000,
                :connections => [
                    {:main, [
                      {'dog-ubuntu-server.lxd', 5673 } 
                    ]}
                ]
            }
        ]	
