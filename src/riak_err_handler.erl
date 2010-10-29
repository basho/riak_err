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

format_event(Event, #state{max_len = MaxLen}) ->
    ReportStr = case Event of
                    {error, _GL, _}                         -> "ERROR REPORT";
                    %% SLF: non-standard string below.
                    {emulator, _, _}                        -> "ERROR REPORT (emulator)";
                    {error_report, _, {_, std_error, _}}    -> "ERROR REPORT";
                    {info, _, _}                            -> "INFO REPORT";
                    {info_report, _, {_, std_info, _}}      -> "INFO REPORT";
                    {info_msg, _, _}                        -> "INFO REPORT";
                    {warning_report, _, {_, std_warning, _}}-> "WARNING REPORT";
                    {warning_msg, _, _}                     -> "WARNING REPORT";
                    %% This type is ignored, so whatever.
                    _                                       -> "ODD REPORT"
                end,
    Time = write_time(maybe_utc(erlang:localtime()), ReportStr),
    {Str, _} = trunc_io:print(Event, MaxLen),
    io_lib:format("~s: ~s: event str ~s\n", [Time, ?MODULE, Str]).

%% From OTP stdlib's error_logger_tty_h.erl ... the !@#$! functions
%% aren't exported.

write_time({utc,{{Y,Mo,D},{H,Mi,S}}},Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
                  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({{Y,Mo,D},{H,Mi,S}},Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
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
