-module(diff).

-include("dog_trainer.hrl").

-export([delta_map/2, delta_text/2,
        diff_text/2, diff_git/2, diff_changes/2]).

-spec delta_map(map(), map()) -> {'ok', iolist()}.
delta_map(A, B) ->
    {ok, maps_utils:diff(A,B)}.

-spec diff_text(iolist(), iolist()) -> {'ok', iolist()}.
diff_text(A, B) ->
    {ok, TempFileA } = write_to_temp_file(A),
    {ok, TempFileB } = write_to_temp_file(B),
    Cmd = io_lib:format("diff -y ~s ~s", [TempFileA,TempFileB]),
    Result = os:cmd(Cmd),
    ok = delete_tempfile(TempFileA),
    ok = delete_tempfile(TempFileB),
    {ok, Result}.

-spec diff_git(iolist(), iolist()) -> {'ok', iolist()}.
diff_git(A, B) ->
    {ok, TempFileA } = write_to_temp_file(A),
    {ok, TempFileB } = write_to_temp_file(B),
    Cmd = io_lib:format("git diff --no-index ~s ~s", [TempFileA,TempFileB]),
    Result = os:cmd(Cmd),
    ok = delete_tempfile(TempFileA),
    ok = delete_tempfile(TempFileB),
    {ok, Result}.

-spec diff_changes(A :: iolist(), B :: iolist()) -> {'ok',iolist()} | {'error',atom()}.
diff_changes(A, B) ->
    {ok, TempFileA } = write_to_temp_file(A),
    {ok, TempFileB } = write_to_temp_file(B),
    Cmd = io_lib:format("git diff --no-index --numstat ~s ~s", [TempFileA,TempFileB]),
    Result = os:cmd(Cmd),
    ?LOG_DEBUG("Result: ~p~s", [Result]),
    Matches = re:run(Result, "(\\d+)\\t(\\d+)\\t.*", [global,{capture, all_but_first, list}]), 
    ?LOG_DEBUG("Matches: ~p~s", [Matches]),
    case Matches of
        {match, [[Adds, Subs]]} ->
            Changes = [list_to_binary(Adds), list_to_binary(Subs)],
            ?LOG_DEBUG("Changes: ~p~s", [Changes]),
            ok = delete_tempfile(TempFileA),
            ok = delete_tempfile(TempFileB),
            {ok, Changes};
        _ ->
            ok = delete_tempfile(TempFileA),
            ok = delete_tempfile(TempFileB),
            {error, git_changes_failed}
    end.

-spec delta_text(iolist(), iolist()) -> {'ok', iolist()}.
delta_text(A, B) ->
    Result = diffy:diff(list_to_binary(A),list_to_binary(B)),
    {ok, Result}.

-spec delete_tempfile(TempFile :: iolist()) -> 'ok'.
delete_tempfile(TempFile) ->
    ok = file:delete(TempFile),
    ok.

-spec write_to_temp_file(iolist()) -> {'ok',[any(),...]}.
write_to_temp_file(String) ->
    RandString = erlang:phash2(make_ref()),
    TempFile = io_lib:format("~s/diff.~B.txt", [?RUNDIR,RandString]),
    io:format("TempFile: ~s~n",[TempFile]),
    ok = file:write_file(TempFile, String),
    {ok, TempFile}.
