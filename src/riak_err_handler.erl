%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Replace the OTP default error_logger's event handler (which
%% can cause memory use problems when handling very large messages)
%% with a handler that will use a limited amount of RAM but is
%% otherwise equivalent.

-module(riak_err_handler).

-behaviour(gen_event).

%% External exports
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          max_len = 4000
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, riak_err_handler}). 

add_handler() ->
    gen_event:add_handler(riak_err_handler, riak_err_handler, []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event(Event, State) ->
    Formatted = format_event(Event, State),
    io:put_chars(Formatted),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

format_event(Event, S) ->
    %% Case clauses appear the same order as error_logger_tty_h:write_event/1.
    {ReportStr, Pid, MsgStr} =
        case Event of
            {_Type, GL, _Msg} when node(GL) /= node() ->
                {ignore, ignore, ignore};
            {error, _GL, {Pid1, Fmt, Args}} ->
                {"ERROR REPORT", Pid1, limited_fmt(Fmt, Args, S)};
            %% SLF: non-standard string below.
            {emulator, _GL, Chars} ->
                {"ERROR REPORT", emulator, Chars};
            {info, _GL, {Pid1, Info, _}} ->
                {"INFO REPORT", Pid1, limited_str(Info, S)};
            {error_report, _GL, {Pid1, std_error, Rep}} ->
                {"ERROR REPORT", Pid1, limited_str(Rep, S)};
            {error_report, _GL, Other} ->
                perhaps_a_sasl_report(error_report, Other, S);
            {info_report, _GL, {Pid1, std_info, Rep}} ->
                {"INFO REPORT", Pid1, limited_str(Rep, S)};
            {info_report, _GL, Other} ->
                perhaps_a_sasl_report(info_report, Other, S);
            {info_msg, _GL, {Pid1, Fmt, Args}} ->
                {"INFO REPORT", Pid1, limited_fmt(Fmt, Args, S)};
            {warning_report, _GL, {Pid1, std_warning, Rep}} ->
                {"WARNING REPORT", Pid1, limited_str(Rep, S)};
            {warning_msg, _GL, {Pid1, Fmt, Args}} ->
                {"WARNING REPORT", Pid1, limited_fmt(Fmt, Args, S)};
            %% This type is allegedly ignored, so whatever.
            _E ->
                {"ODD REPORT", "blahblah", limited_fmt("odd ~p", [_E], S)}
        end,
    if ReportStr == ignore ->
            ok;
       true ->
            Time = write_time(maybe_utc(erlang:localtime()), ReportStr),
            NodeSuffix = other_node_suffix(Pid),
            io_lib:format("~s~s~s", [Time, MsgStr, NodeSuffix])
    end.

limited_fmt(Fmt, Args, _S) ->
    io_lib:format("FIXME: " ++ Fmt, Args).    

limited_str(Term, S) ->
    {Str, _} = trunc_io:print(Term, S#state.max_len),
    Str.

other_node_suffix(Pid) when node(Pid) =/= node() ->
    "** at node " ++ atom_to_list(node(Pid)) ++ " **\n";
other_node_suffix(_) ->
    "".

perhaps_a_sasl_report(error_report, {Pid, Type, Report}, S) ->
    case is_my_error_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, S)};
        false ->
            {ignore, ignore, ignore}
    end;
perhaps_a_sasl_report(info_report, {Pid, Type, Report}, S) ->
    case is_my_info_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, S)};
        false ->
            {ignore, ignore, ignore}
    end;
perhaps_a_sasl_report(_, _, _) ->
    {ignore, ignore, ignore}.

sasl_type_to_report_head(supervisor_report) ->
    "SUPERVISOR REPORT";
sasl_type_to_report_head(crash_report) ->
    "CRASH REPORT";
sasl_type_to_report_head(progress) ->
    "PROGRESS REPORT".

sasl_limited_str(supervisor_report, Report, _S) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    FmtString = "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
        "~80.18p~n     Offender:   ~80.18p~n~n",
    FmtString = "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
        "~80.18p~n     Offender:   ~80.18p~n~n",
    io_lib:format("FixMe: " ++ FmtString, [Name, Context, Reason, Offender]);
sasl_limited_str(progress, Report, _S) ->
    [io_lib:format("FixMe    ~16w: ~p~n",[Tag,Data]) || {Tag, Data} <- Report];
sasl_limited_str(crash_repofg, Report, _S) ->
    ["FixMe2: ", proc_lib:format(Report)].

%% From OTP stdlib's error_logger_tty_h.erl ... the !@#$! functions
%% aren't exported.

write_time({utc,{{Y,Mo,D},{H,Mi,S}}},Type) ->
    %% TODO PUT ME BACK: io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
    io_lib:format("~n-~s-==- ~p-~s-~p::~s:~s:~s UTC -=-~n",
                  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({{Y,Mo,D},{H,Mi,S}},Type) ->
    %% TODO PUT ME BACK: io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
    io_lib:format("~n-~s-==- ~p-~s-~p::~s:~s:~s -=-~n",
                  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).

maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} ->
                  Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} ->
                          Val;
                      undefined ->
                          false
                  end
          end,
    if
        UTC =:= true ->
            {utc, calendar:local_time_to_universal_time_dst(Time)};
        true -> 
            Time
    end.

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% From OTP sasl's sasl_report.erl ... the !@#$! functions
%% aren't exported.

is_my_error_report(supervisor_report)   -> true;
is_my_error_report(crash_report)        -> true;
is_my_error_report(_)                   -> false.

is_my_info_report(progress)  -> true;
is_my_info_report(_)         -> false.

sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
        {value, {_, Value}} ->
            Value;
        _ ->
            ""
    end.
