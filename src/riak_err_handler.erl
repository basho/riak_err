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

%% @doc A memory-limited info/error/warning event handler.
%%
%% Replace the OTP default error_logger's event handler (which
%% can cause memory use problems when handling very large messages)
%% with a handler that will use a limited amount of RAM but is
%% otherwise equivalent.
%%
%% If the SASL error logger's <tt>sasl_error_logger</tt> configuration
%% parameter is set to the <tt>{file, FileName}</tt> form, then this
%% module will attempt to emulate the SASL error logger's
%% logging-to-file behavior.  However, the interpretation of the
%% <tt>errlog_type</tt> configuration parameter is limited: if its
%% value is <tt>error</tt>, then only error and warning messages will
%% be written to the file.  In all other cases (namely
%% <tt>progress</tt> and <tt>all</tt>), all events will be formatted
%% and written to the file.
%% <ul>
%% <li> <b>NOTE:</b> The log file's filehandle will be re-opened once
%%                   per second, which will allow log file rotation schemes
%%                   to rotate the log file safely without undue worry about
%%                   losing log file entries or worrying about sending a
%%                   SIGHUP signal to the owner process before rotation. </li>
%% </ul>

-module(riak_err_handler).

-behaviour(gen_event).

%% External exports
-export([add_sup_handler/0,
         set_term_max_size/1, set_fmt_max_bytes/1, reopen_log_file/0,
         get_state/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          term_max_size,
          fmt_max_bytes,
          log_path,
          log_fh,
          errlog_type
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Add a supervised handler to the OTP kernel's
%%      <tt>error_logger</tt> event server.

add_sup_handler() ->
    gen_event:add_sup_handler(error_logger, ?MODULE, []).

%% @doc Change the internal value of <tt>set_term_max_size</tt>.

set_term_max_size(Num) ->
    gen_event:call(error_logger, ?MODULE, {set_term_max_size, Num}, infinity).

%% @doc Change the internal value of <tt>set_fmt_max_bytes</tt>.

set_fmt_max_bytes(Num) ->
    gen_event:call(error_logger, ?MODULE, {set_fmt_max_bytes, Num}, infinity).

%% @doc Tell our error handler to reopen the <tt>sasl_error_logger</tt> file's
%%      file handle (e.g., to assist log file rotation schemes).

reopen_log_file() ->
    gen_event:call(error_logger, riak_err_handler, reopen_log_file, infinity).

%% @doc Debugging: get internal state record.

get_state() ->
    gen_event:call(error_logger, ?MODULE, {get_state}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([]) ->
    TermMaxSize = get_int_env(term_max_size, 10*1024),
    FmtMaxBytes = get_int_env(fmt_max_bytes, 12*1024),
    {LogPath, LogFH} = case application:get_env(sasl, sasl_error_logger) of
                           {ok, {file, Path}} ->
                               FH = open_log_file(Path),
                               {Path, FH};
                           _ ->
                               {undefined, undefined}
                       end,
    ErrlogType = case application:get_env(sasl, errlog_type) of
                     {ok, error} -> error;
                     _           -> all
                 end,
    {ok, #state{term_max_size = TermMaxSize,
                fmt_max_bytes = FmtMaxBytes,
                log_path = LogPath,
                log_fh = LogFH,
                errlog_type = ErrlogType}}.

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
handle_call(reopen_log_file, State) ->
    case State#state.log_fh of
        undefined ->
            {ok, ok, State};
        _ ->
            catch file:close(State#state.log_fh),
            FH = open_log_file(State#state.log_path),
            {ok, ok, State#state{log_fh = FH}}
    end;
handle_call({set_term_max_size, Num}, State) ->
    {ok, ok, State#state{term_max_size = Num}};
handle_call({set_fmt_max_bytes, Num}, State) ->
    {ok, ok, State#state{fmt_max_bytes = Num}};
handle_call({get_state}, State) ->
    {ok, State, State};
handle_call(_Request, State) ->
    Reply = nosupported,
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
    {ReportStr, Pid, MsgStr, ErrorP} =
        case Event of
            {_Type, GL, _Msg} when node(GL) /= node() ->
                {ignore, ignore, ignore, false};
            {error, _GL, {Pid1, Fmt, Args}} ->
                {"ERROR REPORT", Pid1, limited_fmt(Fmt, Args, S), true};
            %% SLF: non-standard string below.
            {emulator, _GL, Chars} ->
                {"ERROR REPORT", emulator, Chars, false};
            {info, _GL, {Pid1, Info, _}} ->
                {"INFO REPORT", Pid1, limited_str(Info, S), false};
            {error_report, _GL, {Pid1, std_error, Rep}} ->
                {"ERROR REPORT", Pid1, limited_str(Rep, S), true};
            {error_report, _GL, Other} ->
                perhaps_a_sasl_report(error_report, Other, S);
            {info_report, _GL, {Pid1, std_info, Rep}} ->
                {"INFO REPORT", Pid1, limited_str(Rep, S), false};
            {info_report, _GL, Other} ->
                perhaps_a_sasl_report(info_report, Other, S);
            {info_msg, _GL, {Pid1, Fmt, Args}} ->
                {"INFO REPORT", Pid1, limited_fmt(Fmt, Args, S), false};
            {warning_report, _GL, {Pid1, std_warning, Rep}} ->
                {"WARNING REPORT", Pid1, limited_str(Rep, S), true};
            {warning_msg, _GL, {Pid1, Fmt, Args}} ->
                {"WARNING REPORT", Pid1, limited_fmt(Fmt, Args, S), true};
            %% This type is allegedly ignored, so whatever.
            _E ->
                {"ODD REPORT", "blahblah", limited_fmt("odd ~p", [_E], S), false}
        end,
    if ReportStr == ignore ->
            "";
       true ->
            Time = riak_err_stdlib:write_time(riak_err_stdlib:maybe_utc(erlang:localtime()), ReportStr),
            NodeSuffix = other_node_suffix(Pid),
            if ErrorP orelse S#state.errlog_type /= error ->
                    file:write(S#state.log_fh, [Time, MsgStr, NodeSuffix]);
               true ->
                    ok
            end,
            io_lib:format("~s~s~s", [Time, MsgStr, NodeSuffix])
    end.

limited_fmt(Fmt, Args, #state{fmt_max_bytes = FmtMaxBytes,
                              term_max_size = TermMaxSize}) ->
    TermSize = erts_debug:flat_size(Args),
    if TermSize > TermMaxSize ->
            {Str, _} = trunc_io:print(Args, FmtMaxBytes),
            ["Oversize args for format \"", Fmt, "\": ", Str];
       true ->
            io_lib:format(Fmt, Args)
    end.

limited_str(Term, S) ->
    {Str, _} = trunc_io:print(Term, S#state.fmt_max_bytes),
    Str.

other_node_suffix(Pid) when node(Pid) =/= node() ->
    "** at node " ++ atom_to_list(node(Pid)) ++ " **\n";
other_node_suffix(_) ->
    "".

perhaps_a_sasl_report(error_report, {Pid, Type, Report}, S) ->
    case riak_err_stdlib:is_my_error_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, S), true};
        false ->
            {ignore, ignore, ignore, false}
    end;
perhaps_a_sasl_report(info_report, {Pid, Type, Report}, S) ->
    case riak_err_stdlib:is_my_info_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, S), false};
        false ->
            {ignore, ignore, ignore, false}
    end;
perhaps_a_sasl_report(_, _, _) ->
    {ignore, ignore, ignore, false}.

sasl_type_to_report_head(supervisor_report) ->
    "SUPERVISOR REPORT";
sasl_type_to_report_head(crash_report) ->
    "CRASH REPORT";
sasl_type_to_report_head(progress) ->
    "PROGRESS REPORT".

sasl_limited_str(supervisor_report, Report,
                 #state{fmt_max_bytes = FmtMaxBytes}) ->
    Name = riak_err_stdlib:sup_get(supervisor, Report),
    Context = riak_err_stdlib:sup_get(errorContext, Report),
    Reason = riak_err_stdlib:sup_get(reason, Report),
    Offender = riak_err_stdlib:sup_get(offender, Report),
    FmtString = "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
        "~s~n     Offender:   ~s~n~n",
    {ReasonStr, _} = trunc_io:print(Reason, FmtMaxBytes),
    {OffenderStr, _} = trunc_io:print(Offender, FmtMaxBytes),
    io_lib:format(FmtString, [Name, Context, ReasonStr, OffenderStr]);
sasl_limited_str(progress, Report, #state{fmt_max_bytes = FmtMaxBytes}) ->
    [begin
         {Str, _} = trunc_io:print(Data, FmtMaxBytes),
         io_lib:format("    ~16w: ~s~n", [Tag, Str])
     end || {Tag, Data} <- Report];
sasl_limited_str(crash_report, Report, #state{fmt_max_bytes = FmtMaxBytes}) ->
    riak_err_stdlib:proc_lib_format(Report, FmtMaxBytes).

get_int_env(Name, Default) ->
    case application:get_env(riak_err, Name) of
        {ok, Val} ->
            Val;
        _ ->
            get_int_env2(Name, Default)
    end.            

get_int_env2(Name, Default) when is_atom(Name) ->
    get_int_env2(atom_to_list(Name), Default);
get_int_env2(Name, Default) ->
    case init:get_argument(riak_err) of
        {ok, ListOfLists} ->
            find_int_in_pairs(lists:append(ListOfLists), Name, Default);
        error ->
            Default
    end.

find_int_in_pairs([Key, Value|_], Key, _Default) ->
    list_to_integer(Value);
find_int_in_pairs([_K, _V|Tail], Key, Default) ->
    find_int_in_pairs(Tail, Key, Default);
find_int_in_pairs(_, _, Default) ->
    Default.

open_log_file(Path) ->
    {ok, FH} = file:open(Path, [append, raw, binary]),
    FH.
