-module(ibrowse_pool).

-include_lib("ibrowse/include/ibrowse.hrl").

-include("ibrowse_pool_spec.hrl").

-behaviour(gen_server).

-export_type(
    [ option/0
    , options/0
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

%% ============================================================================
%% Internal data
%% ============================================================================

-record(state,
    { spec :: ibrowse_pool_spec:t()
    , lb   :: none | {some, pid()}
    }).

-type state() ::
    #state{}.

-record(req_params,
    { url               :: #url{}
    , headers           :: [{string(), string()}]
    , method            :: atom()
    , body              :: string()
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
start_link(#ibrowse_pool_spec{name=Name}=PoolSpec) ->
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
    case catch ibrowse_lib:parse_url(UrlRaw)
    of  #url{}=UrlParsed ->
            ReqParams =
                #req_params
                { url     = UrlParsed
                , headers = Headers
                , method  = Method
                , body    = Body
                },
            gen_server:call(Name, {send_req, ReqParams})
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
    State0 =
        #state
        { spec = PoolSpec
        , lb   = none
        },
    {ok, State0}.

terminate(_Reason, #state{lb=none}) ->
    ok;
terminate(_Reason, #state{lb={some, Pid}}) ->
    ok = ibrowse_lb:stop(Pid).

handle_call({send_req, #req_params{}=ReqParams}, _, #state{}=State0) ->
    {State1, Result} = ibrowse_send_req(State0, ReqParams),
    {reply, Result, State1}.

handle_cast(_, #state{}=State) ->
    {noreply, State}.

handle_info(_, #state{}=State) ->
    {noreply, State}.

%% ============================================================================
%%  Internal
%% ============================================================================

-spec state_ensure_lb(state()) ->
    state().
state_ensure_lb(#state{lb={some, Pid}}=State0) when is_pid(Pid) ->
    State0;
state_ensure_lb(#state{lb=none, spec=#ibrowse_pool_spec{}=Spec}=State0) ->
    #ibrowse_pool_spec
    { host = Host
    , port = Port
    } = Spec,
    {ok, Pid} = ibrowse_lb:start_link([Host, Port]),
    State0#state{lb = {some, Pid}}.

ibrowse_send_req(#state{spec=Spec}=State0, ReqParams) ->
    #ibrowse_pool_spec
    { host              = _
    , port              = _
    , ssl               = SSLOpt
    , max_sessions      = Max_sessions
    , max_pipeline_size = Max_pipeline_size
    , max_attempts      = Max_attempts
    , timeout           = Timeout
    } = Spec,
    #req_params
    { url     = Url
    , headers = Headers
    , method  = Method
    , body    = Body
    } = ReqParams,
    {IsSSL, SSLOptions} =
        case SSLOpt
        of  none -> {false, []}
        ;   {some, SSLOptions0} -> {true, SSLOptions0}
        end,
    #state{lb={some, LbPid}}=State1 = state_ensure_lb(State0),
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
