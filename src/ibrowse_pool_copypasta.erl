%%% ----------------------------------------------------------------------------
%%%  (Mostly) copy-pasta from ibrowse.erl:
%%% ----------------------------------------------------------------------------
%%%  Copyright (c) 2005-2014, Chandrashekhar Mullaparthi
%%%  All rights reserved.
%%%
%%%  Redistribution and use in source and binary forms, with or without
%%%  modification, are permitted provided that the following conditions are met:
%%%
%%%      * Redistributions of source code must retain the above copyright
%%%        notice, this list of conditions and the following disclaimer.
%%%      * Redistributions in binary form must reproduce the above copyright
%%%        notice, this list of conditions and the following disclaimer in the
%%%        documentation and/or other materials provided with the distribution.
%%%      * Neither the name of the T-Mobile nor the names of its contributors
%%%        may be used to endorse or promote products derived from this software
%%%        without specific prior written permission.
%%%
%%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%%  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%%%  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%%  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%%  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%%  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%%  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%%  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%%  POSSIBILITY OF SUCH DAMAGE.

-module(ibrowse_pool_copypasta).

-export(
    [ try_routing_request/14
    ]).

try_routing_request(
    Lb_pid,
    Url,
    Max_sessions,
    Max_pipeline_size,
    {SSLOptions, IsSSL},
    Headers,
    Method,
    Body,
    Options,
    Timeout,
    Ori_timeout,
    Req_start_time,
    Max_attempts,
    Try_count
) when Try_count < Max_attempts ->
    ProcessOptions = ibrowse_lib:get_value(worker_process_options, Options, []),
    case ibrowse_lb:spawn_connection(Lb_pid, Url,
                                             Max_sessions,
                                             Max_pipeline_size,
                                             {SSLOptions, IsSSL},
                                             ProcessOptions) of
        {ok, {_Pid_cur_spec_size, _, Conn_Pid}} ->
            case do_send_req(Conn_Pid, Url, Headers,
                             Method, Body, Options, Timeout) of
                {error, sel_conn_closed} ->
                    Time_now = os:timestamp(),
                    Time_taken_so_far = trunc(round(timer:now_diff(Time_now, Req_start_time)/1000)),
                    Time_remaining = Ori_timeout - Time_taken_so_far,
                    Time_remaining_percent = trunc(round((Time_remaining/Ori_timeout)*100)),
                    %% io:format("~p -- Time_remaining: ~p (~p%)~n", [self(), Time_remaining, Time_remaining_percent]),
                    case (Time_remaining > 0) andalso (Time_remaining_percent >= 5) of
                        true ->
                            try_routing_request(Lb_pid, Url,
                                                Max_sessions,
                                                Max_pipeline_size,
                                                {SSLOptions, IsSSL},
                                                Headers, Method, Body, Options,
                                                Time_remaining, Ori_timeout, Req_start_time, Max_attempts, Try_count + 1);
                        false ->
                            {error, retry_later}
                    end;
                OkResult ->
                    OkResult
            end;
        ErrorResult ->
            ErrorResult
    end;
try_routing_request(_, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    {error, retry_later}.

do_send_req(Conn_Pid, Parsed_url, Headers, Method, Body, Options, Timeout) ->
    case catch ibrowse_http_client:send_req(Conn_Pid, Parsed_url,
                                            Headers, Method, ensure_bin(Body),
                                            Options, Timeout) of
        {'EXIT', {timeout, _}} ->
            P_info = case catch erlang:process_info(Conn_Pid, [messages, message_queue_len, backtrace]) of
                            [_|_] = Conn_Pid_info_list ->
                                Conn_Pid_info_list;
                            _ ->
                                process_info_not_available
                        end,
            (catch lager:error("{ibrowse_http_client, send_req, ~1000.p} gen_server call timeout.~nProcess info: ~p~n",
                               [[Conn_Pid, Parsed_url, Headers, Method, Body, Options, Timeout], P_info])),
            {error, req_timedout};
        {'EXIT', {normal, _}} = Ex_rsn ->
            (catch lager:error("{ibrowse_http_client, send_req, ~1000.p} gen_server call got ~1000.p~n",
                              [[Conn_Pid, Parsed_url, Headers, Method, Body, Options, Timeout], Ex_rsn])),
            {error, req_timedout};
        {error, X} when X == connection_closed;
                        X == {send_failed, {error, enotconn}};
                        X == {send_failed,{error,einval}};
                        X == {send_failed,{error,closed}};
                        X == connection_closing;
                        ((X == connection_closed_no_retry) andalso ((Method == get) orelse (Method == head))) ->
            {error, sel_conn_closed};
        {error, connection_closed_no_retry} ->
            {error, connection_closed};
        {error, {'EXIT', {noproc, _}}} ->
            {error, sel_conn_closed};
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        {ok, St_code, Headers, Body} = Ret when is_binary(Body) ->
            case ibrowse_lib:get_value(response_format, Options, list) of
                list ->
                    {ok, St_code, Headers, binary_to_list(Body)};
                binary ->
                    Ret
            end;
        {ok, St_code, Headers, Body, Req} = Ret when is_binary(Body) ->
            case ibrowse_lib:get_value(response_format, Options, list) of
                list ->
                    {ok, St_code, Headers, binary_to_list(Body), Req};
                binary ->
                    Ret
            end;
        Ret ->
            Ret
    end.

ensure_bin(L) when is_list(L)                     -> list_to_binary(L);
ensure_bin(B) when is_binary(B)                   -> B;
ensure_bin(Fun) when is_function(Fun)             -> Fun;
ensure_bin({Fun}) when is_function(Fun)           -> Fun;
ensure_bin({Fun, _} = Body) when is_function(Fun) -> Body.
