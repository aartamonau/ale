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

-module(ale).

-behaviour(gen_server).

-export([start_link/0,
         start_sink/3, start_sink/4, stop_sink/1,
         start_logger/1, start_logger/2,
         stop_logger/1,
         add_sink/2, add_sink/3,
         set_loglevel/2, get_loglevel/1,
         set_sink_loglevel/3, get_sink_loglevel/2,
         sync_sink/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({parse_transform, ale_transform}).

-include("ale.hrl").

-record(state, {sinks   :: dict(),
                loggers :: dict()}).

-record(logger, {name      :: atom(),
                 loglevel  :: loglevel(),
                 sinks     :: dict(),
                 formatter :: module()}).

-record(sink, {name     :: atom(),
               loglevel :: loglevel()}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_sink(Name, Module, Args) ->
    start_sink(Name, ?DEFAULT_SINK_TYPE, Module, Args).

start_sink(Name, Type, Module, Args) ->
    gen_server:call(?MODULE, {start_sink, Name, Type, Module, Args}).

stop_sink(Name) ->
    gen_server:call(?MODULE, {stop_sink, Name}).

start_logger(Name) ->
    start_logger(Name, ?DEFAULT_LOGLEVEL).

start_logger(Name, LogLevel) ->
    start_logger(Name, LogLevel, ?DEFAULT_FORMATTER).

start_logger(Name, LogLevel, Formatter) ->
    gen_server:call(?MODULE, {start_logger, Name, LogLevel, Formatter}).

stop_logger(Name) ->
    gen_server:call(?MODULE, {stop_logger, Name}).

add_sink(LoggerName, SinkName) ->
    add_sink(LoggerName, SinkName, debug).

add_sink(LoggerName, SinkName, LogLevel) ->
    gen_server:call(?MODULE, {add_sink, LoggerName, SinkName, LogLevel}).

set_loglevel(LoggerName, LogLevel) ->
    gen_server:call(?MODULE, {set_loglevel, LoggerName, LogLevel}).

get_loglevel(LoggerName) ->
    gen_server:call(?MODULE, {get_loglevel, LoggerName}).

set_sink_loglevel(LoggerName, SinkName, LogLevel) ->
    gen_server:call(?MODULE,
                    {set_sink_loglevel, LoggerName, SinkName, LogLevel}).

get_sink_loglevel(LoggerName, SinkName) ->
    gen_server:call(?MODULE, {get_sink_loglevel, LoggerName, SinkName}).

sync_sink(SinkName) ->
    try
        gen_server:call(ale_utils:sink_id(SinkName), sync, infinity)
    catch
        exit:{noproc, _} ->
            {error, unknown_sink}
    end.

%% Callbacks
init([]) ->
    State = #state{sinks=dict:new(),
                   loggers=dict:new()},

    {ok, State1} = do_start_logger(?ERROR_LOGGER,
                                   ?DEFAULT_LOGLEVEL, ?DEFAULT_FORMATTER, State),
    {ok, State2} = do_start_logger(?ALE_LOGGER,
                                   ?DEFAULT_LOGLEVEL, ?DEFAULT_FORMATTER, State1),

    set_error_logger_handler(),

    {ok, State2}.

handle_call({start_sink, Name, Type, Module, Args}, _From, State) ->
    RV = do_start_sink(Name, Type, Module, Args, State),
    handle_result(RV, State);

handle_call({stop_sink, Name}, _From, State) ->
    RV = do_stop_sink(Name, State),
    handle_result(RV, State);

handle_call({start_logger, Name, LogLevel, Formatter}, _From, State) ->
    RV = do_start_logger(Name, LogLevel, Formatter, State),
    handle_result(RV, State);

handle_call({stop_logger, Name}, _From, State) ->
    RV = do_stop_logger(Name, State),
    handle_result(RV, State);

handle_call({add_sink, LoggerName, SinkName, LogLevel},
            _From, State) ->
    RV = do_add_sink(LoggerName, SinkName, LogLevel, State),
    handle_result(RV, State);

handle_call({set_loglevel, LoggerName, LogLevel}, _From, State) ->
    RV = do_set_loglevel(LoggerName, LogLevel, State),
    handle_result(RV, State);

handle_call({get_loglevel, LoggerName}, _From, State) ->
    RV = do_get_loglevel(LoggerName, State),
    handle_result(RV, State);

handle_call({set_sink_loglevel, LoggerName, SinkName, LogLevel},
            _From, State) ->
    RV = do_set_sink_loglevel(LoggerName, SinkName, LogLevel, State),
    handle_result(RV, State);

handle_call({get_sink_loglevel, LoggerName, SinkName}, _From, State) ->
    RV = do_get_sink_loglevel(LoggerName, SinkName, State),
    handle_result(RV, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'gen_event_EXIT', ale_error_logger_handler, Reason}, State)
  when Reason =/= normal,
       Reason =/= shutdown ->
    ale:error(?ALE_LOGGER,
              "ale_reports_handler terminated with reason ~p; restarting",
              [Reason]),

    set_error_logger_handler(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ensure_sink(SinkName, #state{sinks=Sinks} = _State, Fn) ->
    case dict:find(SinkName, Sinks) of
        {ok, _} ->
            Fn();
        error ->
            {error, unknown_sink}
    end.

ensure_logger(LoggerName, #state{loggers=Loggers} = _State, Fn) ->
    case dict:find(LoggerName, Loggers) of
        {ok, Logger} ->
            Fn(Logger);
        error ->
            {error, unknown_logger}
    end.

handle_result(Result, OldState) ->
    case Result of
        {ok, NewState} ->
            {reply, ok, NewState};
        {{ok, RV}, NewState} ->
            {reply, {ok, RV}, NewState};
        _Other ->
            {reply, Result, OldState}
    end.

do_start_sink(Name, Type, Module, Args, #state{sinks=Sinks} = State) ->
    case dict:find(Name, Sinks) of
        {ok, _} ->
            {error, duplicate_sink};
        error ->
            SinkId = ale_utils:sink_id(Name),
            Args1 = [SinkId | Args],

            RV = ale_dynamic_sup:start_child(SinkId, Module, Args1),
            case RV of
                {ok, _} ->
                    NewSinks = dict:store(Name, Type, Sinks),
                    NewState = State#state{sinks=NewSinks},
                    {ok, NewState};
                _Other ->
                    RV
            end
    end.

do_stop_sink(Name, #state{sinks=Sinks} = State) ->
    ensure_sink(
      Name, State,
      fun () ->
              SinkId = ale_utils:sink_id(Name),
              ok = ale_dynamic_sup:stop_child(SinkId),
              NewSinks = dict:erase(Name, Sinks),
              NewState = State#state{sinks=NewSinks},
              {ok, NewState}
      end).

do_start_logger(Name, LogLevel, Formatter, State) ->
    case is_valid_loglevel(LogLevel) of
        true ->
            do_start_logger_tail(Name, LogLevel, Formatter, State);
        false ->
            {error, badarg}
    end.

do_start_logger_tail(Name, LogLevel, Formatter,
                     #state{loggers=Loggers} = State) ->
    case dict:find(Name, Loggers) of
        {ok, _Logger} ->
            {error, duplicate_logger};
        error ->
            Logger = #logger{name=Name,
                             loglevel=LogLevel,
                             sinks=dict:new(),
                             formatter=Formatter},

            {ok, compile(State, Logger)}
    end.

do_stop_logger(Name, #state{loggers=Loggers} = State) ->
    ensure_logger(
      Name, State,
      fun (_Logger) ->
              NewLoggers = dict:erase(Name, Loggers),
              State1 = State#state{loggers=NewLoggers},
              {ok, State1}
      end).

do_add_sink(LoggerName, SinkName, LogLevel, State) ->
    case is_valid_loglevel(LogLevel) of
        true ->
            do_add_sink_tail(LoggerName, SinkName, LogLevel, State);
        false ->
            {error, badarg}
    end.

do_add_sink_tail(LoggerName, SinkName, LogLevel, State) ->
    ensure_logger(
      LoggerName, State,
      fun (#logger{sinks=Sinks} = Logger) ->
              ensure_sink(
                SinkName, State,
                fun () ->
                        Sink = #sink{name=SinkName,
                                     loglevel=LogLevel},

                        NewSinks = dict:store(SinkName, Sink, Sinks),
                        NewLogger = Logger#logger{sinks=NewSinks},
                        NewState = compile(State, NewLogger),

                        {ok, NewState}
                end)
      end).

do_set_loglevel(LoggerName, LogLevel, State) ->
    case is_valid_loglevel(LogLevel) of
        true ->
            do_set_loglevel_tail(LoggerName, LogLevel, State);
        false ->
            {error, badarg}
    end.

do_set_loglevel_tail(LoggerName, LogLevel, State) ->
    ensure_logger(
      LoggerName, State,
      fun (#logger{loglevel=CurrentLogLevel} = Logger) ->
              case LogLevel of
                  CurrentLogLevel ->
                      {ok, State};
                  _ ->
                      NewLogger = Logger#logger{loglevel=LogLevel},
                      NewState = compile(State, NewLogger),

                      {ok, NewState}
              end
      end).

do_get_loglevel(LoggerName, State) ->
    ensure_logger(
      LoggerName, State,
      fun (#logger{loglevel=LogLevel}) ->
              {{ok, LogLevel}, State}
      end).

do_set_sink_loglevel(LoggerName, SinkName, LogLevel, State) ->
    case is_valid_loglevel(LogLevel) of
        true ->
            do_set_sink_loglevel_tail(LoggerName, SinkName, LogLevel, State);
        false ->
            {error, badarg}
    end.

do_set_sink_loglevel_tail(LoggerName, SinkName, LogLevel, State) ->
    ensure_logger(
      LoggerName, State,
      fun (#logger{sinks=Sinks} = Logger) ->
              ensure_sink(
                SinkName, State,
                fun () ->
                        case dict:find(SinkName, Sinks) of
                            {ok, #sink{loglevel=LogLevel}} ->   % bound above
                                {ok, State};
                            {ok, Sink} ->
                                NewSink = Sink#sink{loglevel=LogLevel},

                                NewSinks = dict:store(SinkName, NewSink, Sinks),
                                NewLogger = Logger#logger{sinks=NewSinks},
                                NewState = compile(State, NewLogger),
                                {ok, NewState};
                            error ->
                                {error, bad_sink}
                        end
                end)
      end).

do_get_sink_loglevel(LoggerName, SinkName, State) ->
    ensure_logger(
      LoggerName, State,
      fun (#logger{sinks=Sinks}) ->
              ensure_sink(
                SinkName, State,
                fun () ->
                        case dict:find(SinkName, Sinks) of
                            {ok, #sink{loglevel=LogLevel}} ->
                                LogLevel;
                            error ->
                                {error, bad_sink}
                        end
                end)
      end).

set_error_logger_handler() ->
    ok = gen_event:add_sup_handler(error_logger, ale_error_logger_handler,
                                   [?ERROR_LOGGER]).

compile(#state{sinks=SinkTypes,
               loggers=Loggers} = State,
        #logger{name=LoggerName,
                loglevel=LogLevel,
                formatter=Formatter,
                sinks=Sinks} = Logger) ->
    SinksList =
        dict:fold(
          fun (SinkName,
               #sink{name=SinkName, loglevel=SinkLogLevel},
               Acc) ->
                  SinkId = ale_utils:sink_id(SinkName),
                  {ok, SinkType} = dict:find(SinkName, SinkTypes),
                  [{SinkName, SinkId, SinkLogLevel, SinkType} | Acc]
          end, [], Sinks),

    ok = ale_codegen:load_logger(LoggerName, LogLevel, Formatter, SinksList),

    NewLoggers = dict:store(LoggerName, Logger, Loggers),
    State#state{loggers=NewLoggers}.

is_valid_loglevel(LogLevel) ->
    lists:member(LogLevel, ?LOGLEVELS).
