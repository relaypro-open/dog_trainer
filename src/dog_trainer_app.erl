%%%-------------------------------------------------------------------
%% @doc dog_trainer public API
%% @end
%%%-------------------------------------------------------------------

-module(dog_trainer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).
-export([get_version/0]).
-include("dog_trainer.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = ensure_tmp_dir(),
    dog_trainer_sup:start_link().

%%--------------------------------------------------------------------
prep_stop(_State) ->
    %lager:info("Stopping consumer of inbound queue"),
    %supervisor:terminate_child(dog_thumper_sup, ips),
    lager:info("Waiting for outbound queue to clear"),
    wait_for_queue_empty(),
    lager:info("Stopping dog_trainer").

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
wait_for_queue_empty() ->
    case dog_profile_update_agent:queue_length() of
        0 ->
            true;
        QueueLength ->
            lager:info("Outbound queue length: ~p",[QueueLength]),
            PollTimeMilliseconds = application:get_env(dog_trainer,queue_poll_time_seconds,5) * 1000,
            timer:sleep(PollTimeMilliseconds),
            wait_for_queue_empty()
    end.

get_version() -> 
    {ok, Version} = application:get_env(dog_trainer, version),
    {ok, binary:list_to_bin(Version)}.

ensure_tmp_dir() ->
    case file:make_dir(?RUNDIR) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            Error
    end.
