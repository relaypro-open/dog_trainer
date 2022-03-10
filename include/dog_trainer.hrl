-define(ROOT, "/api").
-define(V2ROOT, "/api/V2").

-define(IpsExchange, <<"ips">>).
-define(IptablesExchange, <<"iptables">>).
-define(ConfigExchange, <<"config">>).

-define(RUNDIR, "/tmp/dog_trainer").

-define(SERVER, ?MODULE).
-define(CMD(S), os:cmd(S)).
-define(POOL, pool1).
-define(DB_NAME, <<"dog">>).

%-define(required(Field, Type), Field = error({field_required, Field}) :: Type).
%-define(optional(Field, Type), Field = none :: none | {some, Type}).

-type ip_hostname() :: binary().
-type ips() :: string().

-record(ips_state, {
			hostname,
      ips
       }).
-type ips_state() :: #ips_state{}.
