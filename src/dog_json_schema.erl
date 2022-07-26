-module(dog_json_schema).

-include("dog_trainer.hrl").

-export([
        get_file/1,
        validate/2,
        validate_all/1
        ]).

-spec get_file(Type :: binary()) -> binary().
get_file(Type) ->
    TypeString = binary:bin_to_list(Type),
    SchemaPath = filename:join([code:priv_dir(dog_trainer), "schema", TypeString, TypeString ++ "-schema.json"]),
    case dog_file:read_file(SchemaPath) of
        {ok, FileContents} -> FileContents;
        Other -> Other
    end.

-spec validate(Type :: binary(), Document :: map()) -> ok | {error, _}.
validate(Type, Document) ->
  try 
    SchemaContents = get_file(Type),
    {ok, SchemaMap} = 'Elixir.Jason':decode(SchemaContents),
    ?LOG_DEBUG("SchemaMap: ~p",[SchemaMap]),
    Schema = 'Elixir.JsonXema':new(SchemaMap),
    %?LOG_DEBUG("Schema: ~p",[Schema]),
    Name = maps:get(<<"name">>, Document,<<"NONE">>),
    Id = maps:get(<<"id">>, Document,<<"NONE">>),
    Validation = 'Elixir.JsonXema':validate(Schema,Document),
    case Validation of
      ok ->
        ?LOG_INFO("Schema Validation: ~p, ~p, ~p: ~p",[Type, Name, Id, Validation]);
      {error, _} ->
        ?LOG_ERROR("Schema Validation: ~p, ~p, ~p: ~p",[Type, Name, Id, Validation])
    end,
    Validation
  catch
    Exception:Reason:StackTrace ->
      ?LOG_ERROR("SchemaType: ~p,  Exception: ~p,  Reason: ~p,  StackTrace: ~p",[Type,Exception,Reason,StackTrace]),
      throw(error)
  end.

-spec validate_all(Type :: binary()) -> ResultMap :: map().
validate_all(Type) ->
  DocumentType = case Type of
                   <<"external">> -> dog_external;
                   <<"group">> -> dog_group;
                   <<"host">> -> dog_host;
                   <<"link">> -> dog_link;
                   <<"profile">> -> dog_profile;
                   <<"service">> -> dog_service;
                   <<"zone">> ->  dog_zone
                 end,
  {ok, Documents}  = DocumentType:get_all(),
  TotalDocuments = length(Documents),
  Result = lists:map(
             fun(Document) -> 
                 Id = maps:get(<<"id">>,Document),
                 {Id, dog_json_schema:validate(Type, element(2,DocumentType:get_by_id(Id))) }
             end, Documents ),
  OnlyErrors = lists:filter(fun(X) -> 
                                case X of
                                  {_,ok} -> false;
                                  _ -> true
                                end
                            end, Result),
  TotalErrorDocuments = length(OnlyErrors),
  ErrorDocuments = maps:from_list(OnlyErrors),
  ResultMap = #{ 
    document_type => Type,
    error_total => TotalErrorDocuments,
    errors => ErrorDocuments,
    total_documents => TotalDocuments
   },
  ?LOG_INFO("Validation Results: ~p",[ResultMap]),
  ResultMap.
