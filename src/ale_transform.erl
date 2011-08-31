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

%% This module partially stolen from lager.

-module(ale_transform).

-include("ale.hrl").

-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    walk_ast([], AST).

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, {Module, _PmodArgs}}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    walk_ast([{function, Line, Name, Arity,
                walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards,
                   walk_body([], Body)}|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform(H) | Acc], T).

transform({call, Line, {remote, _Line1,
                        {atom, _Line2, ale},
                        {atom, _Line3, delay}},
           [Expr]} = _Stmt) ->
    delay(Line, Expr);
transform({call, Line, {remote, Line1,
                        {atom, Line2, ale},
                        {atom, Line3, log}},
           [LoggerArg, {var, _Line5, LogLevelVar} | Args]} = Stmt) ->
    case valid_args(Args) of
        true ->
            case LoggerArg of
                {var, Line4, LoggerNameVar} ->
                    emit_fully_dynamic_logger_call(LoggerNameVar, LogLevelVar,
                                                   Args,
                                                   Line, Line1,
                                                   Line2, Line3, Line4);
                {atom, Line4, LoggerName} ->
                    emit_dynamic_logger_call(LoggerName, LogLevelVar, Args,
                                             Line, Line1, Line2, Line3, Line4);
                _Other ->
                    Stmt
            end;
        false ->
            Stmt
    end;
transform({call, Line, {remote, Line1,
                        {atom, Line2, ale},
                        {atom, Line3, LogLevel}},
           [Arg | Args]} = Stmt) ->
    case valid_loglevel(LogLevel) andalso
        valid_args(extended_loglevel(LogLevel), Args) of
        true ->
            case Arg of
                {atom, Line4, LoggerName} ->
                    emit_logger_call(LoggerName, LogLevel, Args,
                                     Line, Line1, Line2, Line3, Line4);
                _Other ->
                    Stmt
            end;
        false ->
            Stmt
    end;
transform(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform(tuple_to_list(Stmt)));
transform(Stmt) when is_list(Stmt) ->
    [transform(S) || S <- Stmt];
transform(Stmt) ->
    Stmt.

do_emit_logger_call(LoggerName, LogLevelExpr, Args,
                    CallLine, RemoteLine, ModLine, ArgLine) ->
    DelayedArgs = delay_calls(Args),

    {call, CallLine,
     {remote, RemoteLine,
      {atom, ModLine, ale_codegen:logger_impl(LoggerName)},
      LogLevelExpr},
     [{atom, ArgLine, get(module)},
      {atom, ArgLine, get(function)},
      {integer, ArgLine, CallLine} |
      DelayedArgs]}.

emit_logger_call(LoggerName, LogLevel, Args,
                 CallLine, RemoteLine, ModLine, FnLine, ArgLine) ->
    do_emit_logger_call(LoggerName, {atom, FnLine, LogLevel}, Args,
                        CallLine, RemoteLine, ModLine, ArgLine).

emit_dynamic_logger_call(LoggerName, LogLevelVar, Args,
                         CallLine, RemoteLine, ModLine, FnLine, ArgLine) ->
    do_emit_logger_call(LoggerName, {var, FnLine, LogLevelVar}, Args,
                        CallLine, RemoteLine, ModLine, ArgLine).

emit_fully_dynamic_logger_call(LoggerNameVar, LogLevelVar, Args,
                               CallLine, RemoteLine,
                               ModLine, FnLine, ArgLine) ->
    DelayedArgs = delay_calls(Args),

    CallLoggerImpl =
        {call, ArgLine,
         {remote, ArgLine,
          {atom, ArgLine, ale_codegen},
          {atom, ArgLine, logger_impl}},
         [{var, ArgLine, LoggerNameVar}]},

    {call, CallLine,
     {remote, RemoteLine,
      {atom, ModLine, erlang},
      {atom, FnLine, apply}},
     [CallLoggerImpl,
      {var, ArgLine, LogLevelVar},
      {cons, ArgLine,
       {atom, ArgLine, get(module)},
       {cons, ArgLine,
        {atom, ArgLine, get(function)},
        {cons, ArgLine,
         {integer, ArgLine, CallLine},
         list_to_ast_list(ArgLine, DelayedArgs)}}}]}.

list_to_ast_list(Line, []) ->
    {nil, Line};
list_to_ast_list(Line, [H | T]) ->
    {cons, Line, H, list_to_ast_list(Line, T)}.

map_ast_list(_Fn, {nil, _Line} = List) ->
    List;
map_ast_list(Fn, {cons, Line, Head, Tail}) ->
    {cons, Line, Fn(Head), map_ast_list(Fn, Tail)}.

delay(Line, Expr) ->
    SuspFn = {'fun', Line,
              {clauses,
               [{clause, Line, [], [],
                 [Expr]}]}},
    Ref = {call, Line, {remote, Line,
                        {atom, Line, erlang},
                        {atom, Line, make_ref}},
           []},
    Susp = {tuple, Line, [{atom, Line, '_susp'}, Ref, SuspFn]},
    Susp.

delay_call({call, Line, _, _} = Call) ->
    delay(Line, Call);
delay_call(Other) ->
    Other.

delay_calls([Fmt, {cons, _, _, _} = Args]) ->
    [Fmt, map_ast_list(fun delay_call/1, Args)];
delay_calls(Args) ->
    Args.

extended_loglevel(LogLevel) ->
    ExtendedLogLevels = [list_to_atom([$x | atom_to_list(LL)])
                         || LL <- ?LOGLEVELS],
    lists:member(LogLevel, ExtendedLogLevels).

normalize_loglevel(LogLevel) ->
    case extended_loglevel(LogLevel) of
        false ->
            LogLevel;
        true ->
            LogLevelStr = atom_to_list(LogLevel),
            [$x | LogLevelStr1] = LogLevelStr,
            list_to_atom(LogLevelStr1)
    end.

valid_loglevel(LogLevel) ->
    NormLogLevel = normalize_loglevel(LogLevel),
    lists:member(NormLogLevel, ?LOGLEVELS).

valid_args(Args) ->
    valid_args(false, Args).

valid_args(ExtendedCall, Args) ->
    N = length(Args),

    case ExtendedCall of
        false ->
            N =:= 1 orelse N =:= 2;
        true ->
            N =:= 2 orelse N =:= 3
    end.
