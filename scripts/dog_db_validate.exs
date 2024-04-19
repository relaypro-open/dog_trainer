#!/usr/bin/env -S ERL_FLAGS=+B elixir
Mix.install([
      {:rethinkdb, git: "https://github.com/point/rethinkdb-elixir.git"},
      {:ex_json_schema, "~> 0.10.2"},
      {:jason, "~> 1.4"},
      {:tesla, "~> 1.9"}
])

if System.get_env("DEPS_ONLY") == "true" do
  System.halt(0)
  Process.sleep(:infinity)
end

defmodule DogDbValidate do
  require RethinkDB.Lambda
  import RethinkDB.Query
  import RethinkDB.Lambda
  use RethinkDB.Connection
  use Tesla

  @moduledoc """
  <!-- TODO -->

  ## Usage
      
      #ssh $DOG_HOST -L 28015:localhost:28015
      $ dog_db_validate.exs
    
  """

  #@args [help: :boolean]
  def main(_args) do
    #{parsed, args} = OptionParser.parse!(args, strict: @args)
    IO.inspect(check())
    System.stop(1)
  end

  defp check do
    #["host","group","zone","profile","link","service","ruleset"]
    ["host","group","zone","profile","link","service"]
    |> Enum.map(fn type ->
      {type, check(type)}
    end)
  end
  
  defp check(type) do
    schema = get_schema(type)
    #Assumes an SSH local tunnel to get to DB.
    {:ok, conn} = RethinkDB.Connection.start_link([host: "localhost", port: 28015, db: "dog"])
    {:ok, t} = table(type)
    |> map(lambda fn (record) ->
      record
    end) |> RethinkDB.run(conn)
    t.data 
    |> Enum.map(fn record ->
      {ExJsonSchema.Validator.validate(schema, record), record["name"], record["id"]}
    end)
    |> Enum.filter(fn {bool,_name,_id} ->
      bool != :ok
    end)
  end

  defp get_schema(type) do
    #Gets schemas from HEAD of main.  Change if needed.
    schema = Tesla.get!("https://raw.githubusercontent.com/relaypro-open/dog_trainer/main/priv/schema/#{type}/#{type}-schema.json").body 
             |> Jason.decode!
             |> ExJsonSchema.Schema.resolve()
    schema
  end
end

DogDbValidate.main(System.argv())
