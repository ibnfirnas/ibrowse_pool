-module(ibrowse_pool_spec).

-include("ibrowse_pool_spec.hrl").

-export_type(
    [ t/0
    , property/0
    , properties/0
    ]).

-export(
    [ of_props/1
    ]).

-define(T, #?MODULE).

-type t() ::
    ?T{}.

-type property() ::
      {name              , atom()}
    | {ssl_opts          , [term()]}
    | {max_sessions      , pos_integer()}
    | {max_pipeline_size , pos_integer()}
    | {max_attempts      , pos_integer()}
    .

-type properties() ::
    [property()].

-spec of_props(properties()) ->
    t().
of_props(Properties) ->
    GetOpt = fun (K)    -> hope_kv_list:get(Properties, K) end,
    GetDef = fun (K, D) -> hope_option:get(GetOpt(K), D) end,
    Get    = fun (K)    -> {some, V} = GetOpt(K), V end,
    ?T
    % Required params
    { name              = Get(name)
    % Optional params
    , ssl_opts          = GetDef(ssl_opts         , [])
    , max_sessions      = GetDef(max_sessions     , 10)
    , max_pipeline_size = GetDef(max_pipeline_size, 10)
    , max_attempts      = GetDef(max_attempts     , 3)
    }.
