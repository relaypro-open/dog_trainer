# File: mix.exs
# This file was generated from rebar.config
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Dog.Trainer.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dog_trainer,
      version: "1.4.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
       applications: [:amqp_client, :asn1, :cowboy, :turtle, :diffy, :gen_smtp, :inets, :jesse, :maps_utils, :public_key, :rabbit_common, :rethink, :sasl, :ssl, :jsx, :base16, :plists, :imetrics, :json_xema, :nested, :poolboy, :jsn, :observer_cli, :erlcloud, :erlsom, :cache_tab],
       env: [],
       mod: {:dog_trainer_app, []}
    ]
  end

  defp deps do
    [
      {:jesse, ~r/.*/, git: "https://github.com/for-GET/jesse.git", tag: "1.6.1"},
      {:jsx, ~r/.*/, git: "https://github.com/talentdeficit/jsx.git", tag: "v3.1.0", override: true},
      {:rethink, ~r/.*/, git: "https://github.com/relaypro-open/rethink-erlang.git", branch: "master"},
      {:turtle, git: "https://github.com/relaypro-open/turtle.git", branch: "feature/remove_lager"},
      {:cowboy, git: "https://github.com/ninenines/cowboy", tag: "2.9.0"},
      {:ranch, git: "https://github.com/ninenines/ranch", tag: "1.8.0", override: true},
      {:diffy, git: "https://github.com/zotonic/diffy.git", tag: "1.0.0"},
      {:maps_utils, git: "https://github.com/egobrain/maps_utils.git", tag: "0.0.6"},
      {:gen_smtp, git: "https://github.com/Vagabond/gen_smtp.git", tag: "0.15.0"},
      {:base16, git: "https://github.com/goj/base16.git", tag: "1.0.0"},
      {:plists, git: "https://github.com/silviucpp/plists.git", branch: "master"},
      {:imetrics, git: "https://github.com/Phonebooth/imetrics.git", branch: "master"},
      {:erlcloud, git: "https://github.com/erlcloud/erlcloud.git", tag: "3.6.2"},
      {:erlsom, git: "https://github.com/willemdj/erlsom.git", tag: "1.5.0"},
      {:cache_tab, git: "git@github.com:relaypro-open/cache_tab.git", branch: "master"},
      {:recon, git: "https://github.com/ferd/recon.git", tag: "2.5.2", override: true},
      {:observer_cli, "1.6.0"},
      {:jsn, "2.1.4"},
      {:nested, "0.1.2"},
      {:ssl_verify_fun, "1.1.5"},
      {:unicode_util_compat, "0.5.0"},
      #{:xema, "0.9.3"},
      {:json_xema, "0.6.1"},
      {:poolboy, "1.5.2"}
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
      end).(command)
  end

  defp trim(x) do
    if Version.compare(System.version, "1.5.0") == :lt do
      Kernel.apply(String, :strip, [x])
    else
      Kernel.apply(String, :trim, [x])
    end
  end
end
