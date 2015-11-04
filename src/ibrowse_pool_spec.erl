-module(ibrowse_pool_spec).

-include("ibrowse_pool_spec.hrl").

-export_type(
    [ t/0
    ]).

-export(
    [ of_pairs/1
    ]).

-define(T, #?MODULE).

-type t() ::
    ?T{}.

-spec of_pairs([{atom(), term()}]) ->
    t().
of_pairs(Pairs) ->
    GetOpt = fun (K)    -> hope_kv_list:get(Pairs, K) end,
    GetDef = fun (K, D) -> hope_option:get(GetOpt(K), D) end,
    Get    = fun (K)    -> {some, V} = GetOpt(K), V end,
    ?T
    % Required params
    { name              = Get(name)
    , host              = Get(host)
    , port              = Get(port)

    % Optional params
    , ssl               = GetDef(ssl              , none)
    , max_sessions      = GetDef(max_sessions     , 10)
    , max_pipeline_size = GetDef(max_pipeline_size, 10)
    , max_attempts      = GetDef(max_attempts     , 3)
    , timeout           = GetDef(timeout          , 5000)
    }.
