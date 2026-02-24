%%% @doc Top-level supervisor for project_node (PRJ).
%%%
%%% Supervises the SQLite store and all projection desk supervisors.
%%% @end
-module(project_node_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% SQLite connection worker (must start first)
        #{
            id => project_node_store,
            start => {project_node_store, start_link, []},
            restart => permanent,
            type => worker
        },
        %% Projection: plugin_installed_v1 -> plugins table
        #{
            id => plugin_installed_v1_to_plugins_sup,
            start => {plugin_installed_v1_to_plugins_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: plugin_upgraded_v1 -> plugins table
        #{
            id => plugin_upgraded_v1_to_plugins_sup,
            start => {plugin_upgraded_v1_to_plugins_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: plugin_removed_v1 -> plugins table
        #{
            id => plugin_removed_v1_to_plugins_sup,
            start => {plugin_removed_v1_to_plugins_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
