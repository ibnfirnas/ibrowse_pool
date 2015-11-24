-module(ibrowse_pool).

-include_lib("ibrowse/include/ibrowse.hrl").

-include("ibrowse_pool_spec.hrl").

-behaviour(gen_server).

-export_type(
    [ option/0
    , options/0
    , host_port/0
    ]).

%% API
-export(
    [ start_link/1
    , start_supervised/1
    , send_req/5
    ]).

%% gen_server callbacks
-export(
    [ init/1
    , handle_call/3
    , handle_cast/2
    , handle_info/2
    , terminate/2
    , code_change/3
    ]).

-type option() ::
    {atom(), term()}.

-type options() ::
    [option()].

-type host_port() ::
    {string(), inet:port_number()}.

%% ============================================================================
%% Internal data
%% ============================================================================

-record(state,
    { spec :: ibrowse_pool_spec:t()
    , load_balancers :: dict:dict(host_port(), pid())
    }).

-type state() ::
    #state{}.

-record(req_params,
    { url               :: #url{}
    , headers           :: [{string(), string()}]
    , method            :: atom()
    , body              :: string()
    , timeout           :: non_neg_integer()
    , max_sessions      :: pos_integer()
    , max_pipeline_size :: pos_integer()
    , max_attempts      :: pos_integer()
    , ssl_opts          :: options()
    , start_time        :: options()
    }).

%% ============================================================================
%%  API
%% ============================================================================

-spec start_supervised(ibrowse_pool_spec:t()) ->
    supervisor:startchild_ret().
start_supervised(#ibrowse_pool_spec{}=PoolSpec) ->
    ibrowse_pool_sup:start_child(PoolSpec).

-spec start_link(ibrowse_pool_spec:t()) ->
      {ok, pid()}
    | ignore
    | {error, {already_started, pid()} | term()}
    .
start_link(#ibrowse_pool_spec{name=Name, timeout=Timeout}=PoolSpec) ->
    {} = ibrowse_pool_config_stash:set_timeout(Name, Timeout),
    RegisteredName  = Name,
    GenServerModule = ?MODULE,
    GenServerOpts   = [],
    InitArgs        = PoolSpec,
    gen_server:start_link(
        {local, RegisteredName},
        GenServerModule,
        InitArgs,
        GenServerOpts
    ).

-spec send_req(
    atom(),
    string(),
    [{string(), string()}],
    atom(),
    string()
) ->
      {ok, term()}
    | {error, term()}
    .
send_req(Name, UrlRaw, Headers, Method, Body) ->
    Timeout = ibrowse_pool_config_stash:get_timeout(Name),
    case catch ibrowse_lib:parse_url(UrlRaw)
    of  #url{}=UrlParsed ->
            ReqParams =
                #req_params
                { url     = UrlParsed
                , headers = Headers
                , method  = Method
                , body    = Body
                , timeout = Timeout
                },
            try
                gen_server:call(Name, {send_req, ReqParams}, Timeout)
            catch exit:{timeout, _} ->
                {error, req_timedout}
            end
    ;   Error ->
            {error, {url_parsing_failed, Error}}
    end.

%% ============================================================================
%%  gen_server callbacks (unused)
%% ============================================================================

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%%  gen_server callbacks
%% ============================================================================

init(#ibrowse_pool_spec{}=PoolSpec) ->
    erlang:process_flag(trap_exit, true),
    State0 =
        #state
        { spec = PoolSpec
        , load_balancers = dict:new()
        },
    {ok, State0}.

terminate(_Reason, #state{load_balancers=LBs}) ->
    {} = dict:fold(
        fun (_, Pid, {}) ->
                _ = ibrowse_lb:stop(Pid),
                {}
        end,
        {},
        LBs
    ),
    ok.

handle_call({send_req, #req_params{}=ReqParams}, _, #state{}=State0) ->
    {State1, Result} = ibrowse_send_req(State0, ReqParams),
    {reply, Result, State1}.

handle_cast(_, #state{}=State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}=Exit, #state{}=State0) ->
    error_logger:warning_msg("ibrowse_pool trapped exit: ~p~n", [Exit]),
    State1 = state_del_lb_pid(State0, Pid),
    {noreply, State1};
handle_info(_, #state{}=State) ->
    {noreply, State}.

%% ============================================================================
%%  Internal
%% ============================================================================

-spec state_get_lb_pid(state(), string(), inet:port_number()) ->
    {state(), pid()}.
state_get_lb_pid(#state{load_balancers=LBs0}=State0, Host, Port) ->
    HostPort = {Host, Port},
    case dict:find(HostPort, LBs0)
    of  {ok, LB} ->
            {State0, LB}
    ;   error ->
            {ok, LB} = ibrowse_lb:start_link([Host, Port]),
            LBs1 = dict:store(HostPort, LB, LBs0),
            State1 = State0#state{load_balancers = LBs1},
            {State1, LB}
    end.

-spec state_del_lb_pid(state(), pid()) ->
    state().
state_del_lb_pid(#state{load_balancers=LBs0}=State0, PidGiven) ->
    IsNotGivenPid = fun ({_, _}, Pid) -> Pid =/= PidGiven end,
    LBs1 = dict:filter(IsNotGivenPid, LBs0),
    State0#state{load_balancers = LBs1}.

ibrowse_send_req(#state{spec=Spec}=State0, ReqParams) ->
    #ibrowse_pool_spec
    { ssl_opts          = SSLOptions
    , max_sessions      = Max_sessions
    , max_pipeline_size = Max_pipeline_size
    , max_attempts      = Max_attempts
    } = Spec,
    #req_params
    { url     = #url{host=Host, port=Port, protocol=Protocol}=Url
    , headers = Headers
    , method  = Method
    , body    = Body
    , timeout = Timeout
    } = ReqParams,
    IsSSL = Protocol =:= https,
    {State1, LbPid} = state_get_lb_pid(State0, Host, Port),
    Timestamp = os:timestamp(),
    Options = [],
    Result = ibrowse_pool_copypasta:try_routing_request(
        LbPid,
        Url,
        Max_sessions,
        Max_pipeline_size,
        {SSLOptions, IsSSL},
        Headers,
        Method,
        Body,
        Options,
        Timeout,
        Timeout,
        Timestamp,
        Max_attempts,
        0
    ),
    {State1, Result}.
