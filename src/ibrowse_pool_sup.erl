-module(ibrowse_pool_sup).

-include("ibrowse_pool_spec.hrl").

-behaviour(supervisor).

%% API
-export(
    [ start_link/1
    , start_child/1
    ]).

%% Supervisor callbacks
-export([init/1]).

%% ============================================================================
%% API
%% ============================================================================

-spec start_link(ibrowse_pool_spec:t()) ->
    supervisor:startlink_ret().
start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Pools).

-spec start_child(ibrowse_pool_spec:t()) ->
    supervisor:startchild_ret().
start_child(#ibrowse_pool_spec{}=PoolSpec) ->
    Child = child_spec_of_pool_spec(PoolSpec),
    supervisor:start_child(?MODULE, Child).

%% ============================================================================
%% Supervisor callbacks
%% ============================================================================

-spec init([{atom(), term()}]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init(PoolSpecsAsPairs) ->
    PoolSpecs = [ibrowse_pool_spec:of_props(P) || P <- PoolSpecsAsPairs],
    Children  = [child_spec_of_pool_spec(P)    || P <- PoolSpecs],
    {ok, {{one_for_one, 5, 10}, Children}}.

%% ============================================================================
%% Helpers
%% ============================================================================

-spec child_spec_of_pool_spec(ibrowse_pool_spec:t()) ->
    supervisor:child_spec().
child_spec_of_pool_spec(#ibrowse_pool_spec{name=Name}=PoolSpec) ->
    ID       = Name,
    Module   = ibrowse_pool,
    Modules  = [Module],
    MFA      = {Module, start_link, [PoolSpec]},
    Restart  = permanent,
    Shutdown = 5000,
    Type     = worker,
    {ID, MFA, Restart, Shutdown, Type, Modules}.
