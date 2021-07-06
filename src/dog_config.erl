-module(dog_config).

-include("dog_trainer.hrl").

-export([ 
         publish_host_config/1,
         update_host_keepalive/1,
         update_host_hashes/6
        ]).

-spec publish_host_config(Hostkey :: binary()) -> any().
publish_host_config(Hostkey) ->
    {ok, ConfigMap} = dog_host:get_by_hostkey(Hostkey),
    lager:info("ConfigMap: ~p~n",[ConfigMap]),
    UserData = #{config => ConfigMap},
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}]),
    Response = thumper:publish(Message, ?ConfigExchange, Hostkey),
    Response.

-spec update_host_keepalive(Hostkey :: binary() ) -> ok.
update_host_keepalive(Hostkey) ->
  Timestamp = dog_time:timestamp(),
  %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
  %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
  {ok, _R} = dog_rethink:run(
                           fun(X) -> 
                               reql:db(X, dog), 
                               reql:table(X, host),
                               reql:get_all(X, Hostkey, #{index => <<"hostkey">>}),
                               reql:update(X, #{<<"keepalive_timestamp">> => Timestamp})
                           end),
  ok.

-spec update_host_hashes(Hostname :: binary() , Hash4Ipsets :: iolist() ,Hash6Ipsets :: iolist() ,Hash4Iptables :: iolist() ,Hash6Iptables :: iolist(), IpsetHash :: iolist() ) -> ok.
update_host_hashes(Hostname,Hash4Ipsets,Hash6Ipsets,Hash4Iptables,Hash6Iptables,IpsetHash) ->
  lager:debug("Hostname: ~p, Hashes: ~p,~p,~p,~p,~p",[Hostname, Hash4Ipsets,Hash6Ipsets,Hash4Iptables,Hash6Iptables,IpsetHash]),
  %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
  %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
  {ok, _R} = dog_rethink:run(
                            fun(X) -> 
                                reql:db(X, dog), 
                                reql:table(X, host),
                                reql:get_all(X, Hostname, #{index => <<"name">>}),
                                reql:update(X, #{<<"hash4_ipsets">> => Hash4Ipsets, 
                                                 <<"hash6_ipsets">> => Hash6Ipsets,
                                                 <<"hash4_iptables">> => Hash4Iptables,
                                                 <<"hash6_iptables">> => Hash6Iptables,
                                                 <<"ipset_hash">> => IpsetHash})
                            end),
  ok.
