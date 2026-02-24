%%% @doc Hecate Node Plugin Manager top-level supervisor.
%%%
%%% Supervision tree:
%%% hecate_app_noded_sup (one_for_one)
%%%   - app_noded_plugin_registrar (transient worker)
%%%   - Domain app supervisors are started by their own OTP apps
%%% @end
-module(hecate_app_noded_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        #{
            id => app_noded_plugin_registrar,
            start => {app_noded_plugin_registrar, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
