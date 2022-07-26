-module(dog_external_db_agent).
-behaviour(gen_server).

-include("dog_trainer.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
         add_entry/3,
         add_env/1,
         add_group/2,
         get_env/1,
         get_group/2,
         get_env_names/0,
         get_group_names/1,
         remove_entry/3,
         remove_env/1,
         remove_group/2
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_env(Name :: binary()) -> ok | {error, Reason :: atom()}.
add_env(Name) ->
  gen_server:call(?MODULE, {add_env, Name}).

-spec remove_env(Name :: binary()) -> {ok, Env :: map()} | {error, Reason :: atom()}.
remove_env(Name) ->
  gen_server:call(?MODULE, {remove_env, Name}).

-spec get_env(Name:: binary()) -> {ok, Env :: map()} | {error, Reason :: atom()}.
get_env(Name) ->
  gen_server:call(?MODULE, {get_env, Name}).

-spec get_env_names() -> {ok, Envs :: list()}.
get_env_names() ->
  gen_server:call(?MODULE, get_env_names).

-spec add_group(EnvName :: binary(), GroupName :: binary()) -> {ok, Env :: map()} | {error, Reason :: atom()}.
add_group(EnvName, GroupName) ->
  gen_server:call(?MODULE, {add_group, EnvName, GroupName}).

-spec remove_group(EnvName :: binary(), GroupName :: binary()) -> {ok, Env :: map()} | {error, Reason :: atom()}.
remove_group(EnvName, GroupName) ->
  gen_server:call(?MODULE, {remove_group, EnvName, GroupName}).

-spec get_group(EnvName :: binary(), GroupName :: binary()) -> {ok, Group :: list()} | {error, Reason :: atom()}.
get_group(EnvName, GroupName) ->
  gen_server:call(?MODULE, {get_group, EnvName, GroupName}).

-spec get_group_names(EnvName :: binary()) -> {ok, GroupList :: list()} | {error, Reason :: atom()}.
get_group_names(EnvName) ->
  gen_server:call(?MODULE, {get_group_names, EnvName}).

-spec add_entry(EnvName :: binary(), GroupName :: binary(), Entry :: binary()) -> {ok, Group :: list()} | {error, Reason :: atom()}.
add_entry(EnvName, GroupName, Entry) ->
  gen_server:call(?MODULE, {add_entry, EnvName, GroupName, Entry}).

-spec remove_entry(EnvName :: binary(), GroupName :: binary(), Entry :: binary()) -> {ok, Group :: list()} | {error, Reason :: atom()}.
remove_entry(EnvName, GroupName, Entry) ->
  gen_server:call(?MODULE, {remove_entry, EnvName, GroupName, Entry}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init(_) -> {'ok', map()}.
init(_Args) ->
  State = maps:new(),
  {ok, State}.

-spec handle_call(term(), {pid(), term()}, State::ips_state()) -> {reply, ok, any()}.
handle_call({add_env, Name}, _From, State) ->
  list_to_tuple([reply] ++ do_add_env(Name, State));
handle_call({remove_env, Name}, _From, State) ->
  list_to_tuple([reply] ++ do_remove_env(Name, State));
handle_call({get_env, Name}, _From, State) ->
  list_to_tuple([reply] ++ do_get_env(Name, State));
handle_call(get_env_names, _From, State) ->
  list_to_tuple([reply] ++ do_get_env_names(State));
handle_call({add_group, EnvName, GroupName}, _From, State) ->
  list_to_tuple([reply] ++ do_add_group(EnvName, GroupName, State));
handle_call({remove_group, EnvName, GroupName}, _From, State) ->
  list_to_tuple([reply] ++ do_remove_group(EnvName, GroupName, State));
handle_call({get_group, EnvName, GroupName}, _From, State) ->
  list_to_tuple([reply] ++ do_get_group(EnvName, GroupName, State));
handle_call({get_group_names, EnvName}, _From, State) ->
  list_to_tuple([reply] ++ do_get_group_names(EnvName, State));
handle_call({add_entry, EnvName, GroupName, Entry}, _From, State) ->
  list_to_tuple([reply] ++ do_add_entry(EnvName, GroupName, Entry, State));
handle_call({remove_entry, EnvName, GroupName, Entry}, _From, State) ->
  list_to_tuple([reply] ++ do_remove_entry(EnvName, GroupName, Entry, State));
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Msg, State) ->
  ?LOG_ERROR("unknown_message: Msg: ~p, State: ~p",[Msg, State]),
  {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_}.
handle_info(Info, State) ->
  ?LOG_ERROR("unknown_message: Info: ~p, State: ~p",[Info, State]),
  {noreply, State}.

-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
  ?LOG_INFO("terminate: Reason: ~p, State: ~p", [Reason, State]),
  {close}.

-spec code_change(_, State::ips_state(), _) -> {ok, State::ips_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%
do_add_env(Name,State) ->
  case maps:find(Name, State) of
    error ->
      NewState = maps:put(Name, maps:new(), State),
      [{ok,NewState}, NewState];
    _ ->
      [{error, exists}, State]
  end.

do_remove_env(Name,State) ->
  case maps:find(Name,State) of
    error ->
      [{error, not_found}, State];
    _ ->
      NewState = maps:remove(Name,State),
      [{ok,NewState}, NewState]
  end.

do_get_env(Name, State) ->
  case maps:find(Name,State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      [{ok, Env}, State]
  end.

do_get_env_names(State) ->
  [{ok, maps:keys(State)}, State].

do_add_group(EnvName, GroupName, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      case maps:find(GroupName, Env) of
        error ->
          NewEnv = maps:put(GroupName, [], Env),
          NewState = maps:put(EnvName, NewEnv, State),
          [{ok,NewEnv}, NewState];
        _ ->
          [{error, group_exists}, State]
      end
  end.

do_remove_group(EnvName, GroupName, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      NewEnv = maps:remove(GroupName,Env),
      NewState = maps:put(EnvName, NewEnv, State),
      [{ok,NewState}, NewState]
  end.

do_get_group(EnvName, GroupName, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      case maps:find(GroupName, Env) of
        error ->
          [{error, group_not_found}, State];
        {ok, Group} ->
          [{ok, Group}, State]
      end
  end.

do_get_group_names(EnvName, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      [{ok, maps:keys(Env)}, State]
  end.

do_add_entry(EnvName, GroupName, Entry, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      case maps:find(GroupName, Env) of
        error ->
          [{error, group_not_found}, State];
        {ok, Group} ->
          NewGroup = sets:to_list(sets:add_element(Entry,sets:from_list(Group))),
          NewEnv = maps:put(GroupName, NewGroup, Env),
          NewState = maps:put(EnvName, NewEnv, State),
          [{ok, NewGroup}, NewState]
      end
  end.

do_remove_entry(EnvName, GroupName, Entry, State) ->
  case maps:find(EnvName, State) of
    error ->
      [{error, env_not_found}, State];
    {ok, Env} ->
      case maps:find(GroupName, Env) of
        error ->
          [{error, group_not_found}, State];
        {ok, Group} ->
          GroupSet = sets:from_list(Group),
          case sets:is_element(Entry, GroupSet) of
            true ->
              NewGroup = sets:to_list(sets:del_element(Entry, GroupSet)),
              NewEnv = maps:put(GroupName, NewGroup, Env),
              NewState = maps:put(EnvName, NewEnv, State),
              [{ok,NewGroup}, NewState];
            false ->
              {error, entry_not_found}
          end
      end
  end.
