-module(template_setup).
-behaviour(application).

%% application callbacks
-export([start/2,
         stop/1]).

%% API exports
-export([main/0,
         parse_templates/0,
         render/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:start(xmerl),
    application:start(jsx),
    application:start(eini),
    application:start(base16),
    application:start(lhttpc),
    application:start(compiler),
    application:start(erlycloud),
    application:start(erlydtl),
    parse_templates(),
    ok.

stop(_State) ->
    ok.

%% escript Entry point
main() ->
    application:ensure_started(cryto),
    application:ensure_started(public_key),
    application:ensure_started(asn1),
    application:ensure_started(ssl),

    application:ensure_started(compiler),
    application:ensure_started(xmerl),
    application:ensure_started(jsx),
    application:ensure_started(eini),
    application:ensure_started(base16),
    application:ensure_started(lhttpc),
    application:ensure_started(erlycloud),
    application:ensure_started(erlydtl),
    parse_templates().

%%====================================================================
%% Internal functions
%%====================================================================
parse_templates() ->
  io:format("start~n"),
  TemplateDir = "../config/templates/",
  {ok,Files} = file:list_dir(TemplateDir),
  TemplateFileNames = lists:filter(fun(File) -> filename:extension(File) == ".dtl" end, Files),
  TemplateFiles = lists:map(fun(FileName) -> TemplateDir ++ FileName end, TemplateFileNames),
  lists:foreach(fun(TemplateFile) -> render(TemplateFile) end, TemplateFiles),
  io:format("end~n").

render(TemplateFile) ->
  FileName = filename:basename(TemplateFile,".dtl"),
  OutputDir = "../config/",
  OutputFile = OutputDir ++ FileName,
  io:format("Parsing ~s~n",[TemplateFile]),
  {ok,Module} = erlydtl:compile_file(TemplateFile, y, [{libraries,[{dog_erlydtl_lib,dog_erlydtl_lib}] },
                                                               {default_libraries,[dog_erlydtl_lib]},                                                                                                                                              
                                                               {force_recompile}] ),
  {ok,Rendered} = Module:render([],[]),
  io:format("Writing to ~s~n",[OutputFile]),
  file:write_file(OutputFile,Rendered).
