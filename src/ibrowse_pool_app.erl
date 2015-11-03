-module(ibrowse_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Pools = application:get_env(ibrowse_pool, pools, []),
    ibrowse_pool_sup:start_link(Pools).

stop(_State) ->
    ok.
