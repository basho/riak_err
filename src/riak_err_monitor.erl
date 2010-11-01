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

-module(riak_err_monitor).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(Timeout, infinity).

%% External exports
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          max_len = 4000
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

stop() ->
    gen_event:call(?NAME, stop, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    %% Add our custom handler.
    ok = gen_event:add_sup_handler(error_logger, riak_err_handler, []),

    %% Disable the default error logger.
    gen_event:delete_handler(error_logger, error_logger,
                             {stop_please, ?MODULE}),
    %% Disable the SASL default loggers.
    gen_event:delete_handler(error_logger, sasl_report_tty_h,
                             {stop_please, ?MODULE}),
    gen_event:delete_handler(error_logger, sasl_report_file_h,
                             {stop_please, ?MODULE}),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {Str, _} = trunc_io:print(Msg, State#state.max_len),
    io:format("~w: ~s:handle_cast got ~s\n", [self(), ?MODULE, Str]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    %% Our handler ought to be bullet-proof ... but it wasn't, bummer.
    %% We will stop now, and our supervisor will restart us and thus
    %% reinstate the custom event handler.
    {Str, _} = trunc_io:print(Reason, State#state.max_len),
    io:format("~w: ~s: handler ~w exited for reason ~s\n",
              [self(), ?MODULE, Handler, Str]),
    {stop, gen_event_EXIT, State};
handle_info(Info, State) ->
    {Str, _} = trunc_io:print(Info, State#state.max_len),
    io:format("~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Str]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
