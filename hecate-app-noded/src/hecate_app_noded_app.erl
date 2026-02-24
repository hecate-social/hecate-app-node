%%% @doc Hecate Node Plugin Manager daemon application.
%%%
%%% On startup:
%%% 1. Ensures the namespace directory layout exists
%%% 2. Starts ReckonDB infrastructure and node_store
%%% 3. Starts Cowboy on a Unix domain socket
%%% 4. Registers with hecate-daemon (when available)
%%% @end
-module(hecate_app_noded_app).
-behaviour(application).

-include_lib("reckon_db/include/reckon_db.hrl").

-export([start/2, stop/1]).

%% reckon_db is excluded from dialyzer PLT (no debug_info in hex dep beams).
-dialyzer({nowarn_function, start_node_store/0}).

start(_StartType, _StartArgs) ->
    ok = app_noded_paths:ensure_layout(),
    ok = ensure_pg_scope(),
    ok = start_node_store(),
    ok = start_cowboy(),
    logger:info("[hecate_app_noded] Started, socket at ~s",
                [app_noded_paths:socket_path("api.sock")]),
    hecate_app_noded_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(app_noded_http),
    cleanup_socket(),
    ok.

%%% Internal

ensure_pg_scope() ->
    case pg:start(pg) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

start_node_store() ->
    DataDir = app_noded_paths:reckon_path("node"),
    ok = filelib:ensure_path(DataDir),
    Config = #store_config{
        store_id = node_store,
        data_dir = DataDir,
        mode = single,
        writer_pool_size = 5,
        reader_pool_size = 5,
        gateway_pool_size = 2,
        options = #{}
    },
    case reckon_db_sup:start_store(Config) of
        {ok, _Pid} ->
            logger:info("[hecate_app_noded] node_store ready"),
            ok;
        {error, {already_started, _Pid}} ->
            logger:info("[hecate_app_noded] node_store already running"),
            ok;
        {error, Reason} ->
            logger:error("[hecate_app_noded] Failed to start node_store: ~p",
                        [Reason]),
            error({node_store_start_failed, Reason})
    end.

start_cowboy() ->
    SocketPath = app_noded_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath),
    CoreRoutes = [
        {"/health", app_noded_health_api, []},
        {"/manifest", app_noded_manifest_api, []}
    ],
    ApiRoutes = app_noded_api_routes:discover_routes(),
    Dispatch = cowboy_router:compile([{'_', CoreRoutes ++ ApiRoutes}]),
    TransOpts = #{
        socket_opts => [{ifaddr, {local, SocketPath}}],
        num_acceptors => 5
    },
    ProtoOpts = #{
        env => #{dispatch => Dispatch}
    },
    {ok, _} = cowboy:start_clear(app_noded_http, TransOpts, ProtoOpts),
    ok.

cleanup_socket() ->
    SocketPath = app_noded_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath).

cleanup_socket_file(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            logger:warning("[hecate_app_noded] Failed to remove socket ~s: ~p",
                          [Path, Reason]),
            ok
    end.
