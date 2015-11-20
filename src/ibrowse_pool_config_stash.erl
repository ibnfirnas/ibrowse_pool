-module(ibrowse_pool_config_stash).

-export(
    [ start/0
    , set_timeout/2
    , get_timeout/1
    ]).

-define(TABLE, ibrose_pool_config).

-spec start() ->
    {}.
start() ->
    Options =
        [ set
        , public
        , named_table
        , {read_concurrency, true}
        ],
    ?TABLE = ets:new(?TABLE, Options),
    {}.

-spec set_timeout(atom(), timeout()) ->
    {}.
set_timeout(Pool, Value) ->
    set(Pool, timeout, Value).

-spec get_timeout(atom()) ->
    timeout().
get_timeout(Pool) ->
    {some, Timeout} = get(Pool, timeout),
    Timeout.

-spec set(atom(), atom(), term()) ->
    {}.
set(Pool, Key, Value) ->
    true = ets:insert(?TABLE, {{Pool, Key}, Value}),
    {}.

-spec get(atom(), atom()) ->
    hope_option:t(term()).
get(Pool, Key) ->
    case ets:lookup(?TABLE, {Pool, Key})
    of  [] ->
            none
    ;   [{{Pool, Key}, Value}] ->
            {some, Value}
    end.
