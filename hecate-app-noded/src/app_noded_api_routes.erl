%%% @doc Auto-discovery route handler for Node API.
%%%
%%% Discovers all API handlers across Node domain apps by looking
%%% for modules that export routes/0. Delegates incoming requests
%%% to the matching handler.
%%% @end
-module(app_noded_api_routes).

-export([init/2]).
-export([compile/0, discover_routes/0]).

%% Node OTP apps that may contain API handlers.
-define(NODE_APPS, [
    guide_plugin_lifecycle, project_node, query_node
]).

%% @doc Cowboy handler init - dispatches to domain route handlers.
init(Req0, _State) ->
    PathInfo = cowboy_req:path_info(Req0),
    Path = case PathInfo of
        undefined -> cowboy_req:path(Req0);
        Parts -> iolist_to_binary([<<"/">>, lists:join(<<"/">>, Parts)])
    end,
    case find_handler(Path, discover_routes()) of
        {ok, Handler, HandlerState} ->
            Handler:init(Req0, HandlerState);
        nomatch ->
            app_noded_api_utils:not_found(Req0)
    end.

%% @doc Compile all routes into a Cowboy dispatch table.
-spec compile() -> cowboy_router:dispatch_rules().
compile() ->
    cowboy_router:compile([{'_', discover_routes()}]).

%%% Internal

-spec discover_routes() -> [tuple()].
discover_routes() ->
    lists:flatmap(fun collect_app_routes/1, ?NODE_APPS).

collect_app_routes(App) ->
    Mods = app_modules(App),
    Handlers = [M || M <- Mods, M =/= ?MODULE, exports_routes(M)],
    lists:flatmap(fun(M) -> M:routes() end, Handlers).

app_modules(App) ->
    case application:get_key(App, modules) of
        {ok, Mods} -> Mods;
        _ -> []
    end.

exports_routes(Mod) ->
    code:ensure_loaded(Mod),
    erlang:function_exported(Mod, routes, 0).

find_handler(_Path, []) ->
    nomatch;
find_handler(Path, [{Pattern, Handler, HandlerState} | Rest]) ->
    case match_path(Path, Pattern) of
        true -> {ok, Handler, HandlerState};
        false -> find_handler(Path, Rest)
    end;
find_handler(Path, [_ | Rest]) ->
    find_handler(Path, Rest).

match_path(Path, Pattern) when is_binary(Pattern) ->
    Path =:= Pattern;
match_path(Path, Pattern) when is_list(Pattern) ->
    Path =:= list_to_binary(Pattern);
match_path(_, _) ->
    false.
