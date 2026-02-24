%%% @doc Projection: plugin_upgraded_v1 -> plugins table (UPDATE).
-module(plugin_upgraded_v1_to_sqlite_plugins).
-export([project/1]).

-spec project(map()) -> ok | {error, term()}.
project(Event) ->
    PluginId   = app_noded_api_utils:get_field(plugin_id, Event),
    OciImage   = app_noded_api_utils:get_field(oci_image, Event),
    Version    = app_noded_api_utils:get_field(installed_version, Event),
    UpgradedAt = app_noded_api_utils:get_field(upgraded_at, Event),
    Sql = "UPDATE plugins SET "
          "oci_image = ?1, installed_version = ?2, upgraded_at = ?3 "
          "WHERE plugin_id = ?4",
    project_node_store:execute(Sql, [OciImage, Version, UpgradedAt, PluginId]).
