%%% @doc Path utilities for hecate-app-noded.
%%%
%%% Provides purpose-specific path functions for the namespaced
%%% directory layout under ~/.hecate/hecate-app-noded/:
%%%
%%%   sqlite/       - SQLite read-model databases
%%%   reckon-db/    - ReckonDB (Khepri/Ra) event store data
%%%   sockets/      - Unix domain sockets
%%%   run/          - PID and state files
%%%   connectors/   - Connector socket files
%%%
%%% The base directory is configured via
%%% {hecate_app_noded, [{data_dir, Path}]}.
%%% Default: ~/.hecate/hecate-app-noded
%%% @end
-module(app_noded_paths).

-export([
    base_dir/0,
    sqlite_dir/0,
    sqlite_path/1,
    reckon_dir/0,
    reckon_path/1,
    socket_dir/0,
    socket_path/1,
    run_dir/0,
    run_path/1,
    connectors_dir/0,
    gitops_apps_dir/0,
    ensure_layout/0
]).

-spec base_dir() -> file:filename().
base_dir() ->
    case application:get_env(hecate_app_noded, data_dir) of
        {ok, Dir} -> expand_path(Dir);
        undefined -> expand_path("~/.hecate/hecate-app-noded")
    end.

-spec sqlite_dir() -> file:filename().
sqlite_dir() ->
    filename:join(base_dir(), "sqlite").

-spec sqlite_path(string() | binary()) -> file:filename().
sqlite_path(Name) ->
    filename:join(sqlite_dir(), Name).

-spec reckon_dir() -> file:filename().
reckon_dir() ->
    filename:join(base_dir(), "reckon-db").

-spec reckon_path(string() | binary()) -> file:filename().
reckon_path(Name) ->
    filename:join(reckon_dir(), Name).

-spec socket_dir() -> file:filename().
socket_dir() ->
    filename:join(base_dir(), "sockets").

-spec socket_path(string() | binary()) -> file:filename().
socket_path(Name) ->
    filename:join(socket_dir(), Name).

-spec run_dir() -> file:filename().
run_dir() ->
    filename:join(base_dir(), "run").

-spec run_path(string() | binary()) -> file:filename().
run_path(Name) ->
    filename:join(run_dir(), Name).

-spec connectors_dir() -> file:filename().
connectors_dir() ->
    filename:join(base_dir(), "connectors").

-spec gitops_apps_dir() -> file:filename().
gitops_apps_dir() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".hecate", "gitops", "apps"]).

-spec ensure_layout() -> ok.
ensure_layout() ->
    Dirs = [
        sqlite_dir(),
        reckon_dir(),
        socket_dir(),
        run_dir(),
        connectors_dir()
    ],
    lists:foreach(fun(Dir) -> ok = filelib:ensure_path(Dir) end, Dirs).

%%% Internal

expand_path("~/" ++ Rest) ->
    filename:join(os:getenv("HOME"), Rest);
expand_path(Path) ->
    Path.
