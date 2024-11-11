-module(dog_state).

-include("dog_trainer.hrl").

-export([
    from_map/1,
    to_map/1,
    to_routing_key/1
]).

-export([
    dog_state/0,
    dog_state/14,
    get_environment/1,
    get_group/1,
    get_hash4_ipsets/1,
    get_hash4_iptables/1,
    get_hash6_ipsets/1,
    get_hash6_iptables/1,
    get_hostkey/1,
    get_hostname/1,
    get_interfaces/1,
    get_ipset_hash/1,
    get_location/1,
    get_provider/1,
    get_updatetype/1,
    get_version/1,
    set_environment/2,
    set_group/2,
    set_hash4_ipsets/2,
    set_hash4_iptables/2,
    set_hash6_ipsets/2,
    set_hash6_iptables/2,
    set_hostkey/2,
    set_hostname/2,
    set_interfaces/2,
    set_ipset_hash/2,
    set_location/2,
    set_provider/2,
    set_updatetype/2,
    set_version/2
]).

-export([test_state/0]).

-type group() :: binary().
-type hostname() :: binary().
-type location() :: binary().
-type environment() :: binary().
-type hostkey() :: binary().
-type interfaces() :: [tuple()].
-type version() :: binary().
-type hash() :: binary().
-type provider() :: binary().
-type updatetype() :: atom().

-record(dog_state, {
    group,
    name,
    location,
    environment,
    hostkey,
    interfaces,
    version,
    hash4_ipsets,
    hash6_ipsets,
    hash4_iptables,
    hash6_iptables,
    provider,
    updatetype,
    ipset_hash
}).
-type dog_state() :: #dog_state{}.

-spec dog_state() -> dog_state().
dog_state() ->
    dog_state().

dog_state(
    Group,
    Hostname,
    Location,
    Environment,
    Hostkey,
    Interfaces,
    Version,
    Hash4Ipsets,
    Hash6Ipsets,
    Hash4Iptables,
    Hash6Iptables,
    Provider,
    UpdateType,
    IpsetHash
) ->
    #dog_state{
        group = Group,
        name = Hostname,
        location = Location,
        environment = Environment,
        hostkey = Hostkey,
        interfaces = Interfaces,
        version = Version,
        hash4_ipsets = Hash4Ipsets,
        hash6_ipsets = Hash6Ipsets,
        hash4_iptables = Hash4Iptables,
        hash6_iptables = Hash6Iptables,
        provider = Provider,
        updatetype = UpdateType,
        ipset_hash = IpsetHash
    }.

-spec get_group(State :: dog_state()) -> binary().
get_group(State) ->
    State#dog_state.group.

-spec set_group(State :: dog_state(), Group :: group()) -> dog_state().
set_group(State, Group) ->
    State#dog_state{group = Group}.

-spec get_hostname(State :: dog_state()) -> binary().
get_hostname(State) ->
    State#dog_state.name.

-spec set_hostname(State :: dog_state(), Hostname :: hostname()) -> dog_state().
set_hostname(State, Hostname) ->
    State#dog_state{name = Hostname}.

-spec get_location(State :: dog_state()) -> binary().
get_location(State) ->
    State#dog_state.location.

-spec set_location(State :: dog_state(), Location :: location()) -> dog_state().
set_location(State, Location) ->
    State#dog_state{location = Location}.

-spec get_environment(State :: dog_state()) -> binary().
get_environment(State) ->
    State#dog_state.environment.

-spec set_environment(State :: dog_state(), Environment :: environment()) -> dog_state().
set_environment(State, Environment) ->
    State#dog_state{environment = Environment}.

-spec get_hostkey(State :: dog_state()) -> binary().
get_hostkey(State) ->
    State#dog_state.hostkey.

-spec set_hostkey(State :: dog_state(), Hostkey :: hostkey()) -> dog_state().
set_hostkey(State, Hostkey) ->
    State#dog_state{hostkey = Hostkey}.

-spec get_interfaces(State :: dog_state()) -> [tuple()].
get_interfaces(State) ->
    State#dog_state.interfaces.

-spec set_interfaces(State :: dog_state(), Interfaces :: interfaces()) -> dog_state().
set_interfaces(State, Interfaces) ->
    State#dog_state{interfaces = Interfaces}.

-spec get_version(State :: dog_state()) -> binary().
get_version(State) ->
    State#dog_state.version.

-spec set_version(State :: dog_state(), Version :: version()) -> dog_state().
set_version(State, Version) ->
    State#dog_state{version = Version}.

-spec get_hash4_ipsets(State :: dog_state()) -> binary().
get_hash4_ipsets(State) ->
    State#dog_state.hash4_ipsets.

-spec set_hash4_ipsets(State :: dog_state(), Hash4 :: hash()) -> dog_state().
set_hash4_ipsets(State, Hash4) ->
    State#dog_state{hash4_ipsets = Hash4}.

-spec get_hash6_ipsets(State :: dog_state()) -> binary().
get_hash6_ipsets(State) ->
    State#dog_state.hash6_ipsets.

-spec set_hash6_ipsets(State :: dog_state(), Hash6 :: hash()) -> dog_state().
set_hash6_ipsets(State, Hash6) ->
    State#dog_state{hash6_ipsets = Hash6}.

-spec get_hash4_iptables(State :: dog_state()) -> binary().
get_hash4_iptables(State) ->
    State#dog_state.hash4_iptables.

-spec set_hash4_iptables(State :: dog_state(), Hash4 :: hash()) -> dog_state().
set_hash4_iptables(State, Hash4) ->
    State#dog_state{hash4_iptables = Hash4}.

-spec get_hash6_iptables(State :: dog_state()) -> binary().
get_hash6_iptables(State) ->
    State#dog_state.hash6_iptables.

-spec set_hash6_iptables(State :: dog_state(), Hash6 :: hash()) -> dog_state().
set_hash6_iptables(State, Hash6) ->
    State#dog_state{hash6_iptables = Hash6}.

-spec get_provider(State :: dog_state()) -> binary().
get_provider(State) ->
    State#dog_state.provider.

-spec set_provider(State :: dog_state(), Provider :: provider()) -> dog_state().
set_provider(State, Provider) ->
    State#dog_state{provider = Provider}.

-spec get_updatetype(State :: dog_state()) -> atom().
get_updatetype(State) ->
    State#dog_state.updatetype.

-spec set_updatetype(State :: dog_state(), Update :: updatetype()) -> dog_state().
set_updatetype(State, UpdateType) ->
    State#dog_state{updatetype = UpdateType}.

-spec get_ipset_hash(State :: dog_state()) -> binary().
get_ipset_hash(State) ->
    State#dog_state.ipset_hash.

-spec set_ipset_hash(State :: dog_state(), IpsetHash :: hash()) -> dog_state().
set_ipset_hash(State, IpsetHash) ->
    State#dog_state{ipset_hash = IpsetHash}.

to_map(State) ->
    #{
        <<"name">> => State#dog_state.name,
        <<"interfaces">> => State#dog_state.interfaces,
        <<"group">> => State#dog_state.group,
        <<"location">> => State#dog_state.location,
        <<"environment">> => State#dog_state.environment,
        <<"hostkey">> => State#dog_state.hostkey,
        <<"version">> => State#dog_state.version,
        <<"hash4_ipsets">> => State#dog_state.hash4_ipsets,
        <<"hash6_ipsets">> => State#dog_state.hash6_ipsets,
        <<"hash4_iptables">> => State#dog_state.hash4_iptables,
        <<"hash6_iptables">> => State#dog_state.hash6_iptables,
        <<"provider">> => State#dog_state.provider,
        <<"updatetype">> => State#dog_state.updatetype,
        <<"ipset_hash">> => State#dog_state.ipset_hash
    }.

from_map(StateMap) ->
    ?LOG_INFO(#{state_map => StateMap}),
    #dog_state{
        name = maps:get(<<"name">>, StateMap),
        interfaces = maps:get(<<"interfaces">>, StateMap),
        group = maps:get(<<"group">>, StateMap),
        location = maps:get(<<"location">>, StateMap),
        environment = maps:get(<<"environment">>, StateMap),
        hostkey = maps:get(<<"hostkey">>, StateMap),
        version = maps:get(<<"version">>, StateMap),
        hash4_ipsets = maps:get(<<"hash4_ipsets">>, StateMap),
        hash6_ipsets = maps:get(<<"hash6_ipsets">>, StateMap),
        hash4_iptables = maps:get(<<"hash4_iptables">>, StateMap),
        hash6_iptables = maps:get(<<"hash6_iptables">>, StateMap),
        provider = maps:get(<<"provider">>, StateMap),
        updatetype = maps:get(<<"updatetype">>, StateMap),
        ipset_hash = maps:get(<<"ipset_hash">>, StateMap, <<"">>)
    }.

to_routing_key(State) ->
    binary:list_to_bin([
        State#dog_state.environment,
        <<".">>,
        State#dog_state.location,
        <<".">>,
        State#dog_state.group,
        <<".">>,
        State#dog_state.hostkey
    ]).

test_state() ->
    {dog_state, <<"test">>, <<"dog-ubuntu-agent.lxd">>, <<"*">>, <<"*">>, <<"*">>, [
        {<<"lo">>, [<<"127.0.0.1">>, <<"::1">>]},
        {<<"eth0">>, [
            <<"10.216.205.58">>,
            <<"fd42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb">>,
            <<"fe80::216:3eff:fe5c:e3eb">>
        ]},
        {<<"lo:1">>, [<<"127.53.0.1">>]}
    ]}.
