%% @author Couchbase <info@couchbase.com>
%% @copyright 2011 Couchbase, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%      http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ale_stderr_sink).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ale.hrl").

-record(state, {formatter :: atom()}).

start_link(Name) ->
    start_link(Name, []).

start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Opts], []).

init([Opts]) ->
    Formatter = ale_utils:get_formatter(Opts),
    {ok, #state{formatter=Formatter}}.

handle_call({log, Info, Format, Args}, _From,
            #state{formatter=Formatter} = State) ->
    RV = do_log(Formatter, Info, Format, Args),
    {reply, RV, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(Formatter, Info, Format, Args) ->
    Msg = Formatter:format_msg(Info, Format, Args),
    file:write(standard_error, Msg).
