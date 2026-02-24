%%% @doc Plugin manifest endpoint: GET /manifest
%%%
%%% Returns plugin metadata as JSON for hecate-web discovery.
%%% @end
-module(app_noded_manifest_api).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            {ok, Vsn} = application:get_key(hecate_app_noded, vsn),
            Body = json:encode(#{
                <<"name">> => <<"node">>,
                <<"version">> => list_to_binary(Vsn),
                <<"icon">> => <<16#1F5A5/utf8>>,
                <<"description">> => <<"Node Plugin Manager">>,
                <<"tag">> => <<"node-studio">>
            }),
            Req1 = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, Body, Req0),
            {ok, Req1, State};
        _ ->
            Req1 = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, <<"{\"ok\":false,\"error\":\"method_not_allowed\"}">>, Req0),
            {ok, Req1, State}
    end.
