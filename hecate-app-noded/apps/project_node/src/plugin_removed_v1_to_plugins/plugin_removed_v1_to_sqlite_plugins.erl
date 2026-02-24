%%% @doc Projection: plugin_removed_v1 -> plugins table (UPDATE status).
-module(plugin_removed_v1_to_sqlite_plugins).
-export([project/1]).

-spec project(map()) -> ok | {error, term()}.
project(Event) ->
    PluginId  = app_noded_api_utils:get_field(plugin_id, Event),
    RemovedAt = app_noded_api_utils:get_field(removed_at, Event),
    Sql = "UPDATE plugins SET "
          "status = status | 2, status_label = 'Removed', removed_at = ?1 "
          "WHERE plugin_id = ?2",
    project_node_store:execute(Sql, [RemovedAt, PluginId]).
