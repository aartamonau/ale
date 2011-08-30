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
    Transformed =
        case transform(H) of
            {splice, List} ->
                lists:reverse(List);
            Value ->
                [Value]
        end,
    walk_body(Transformed ++ Acc, T).

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
                {cons, Line4, _Head, _Tail} = Cons ->
                    LoggerNames0 =
                        try
                            {ok, ast_atom_list_to_list(Cons)}
                        catch
                            _E:_R ->
                                error
                        end,

                    case LoggerNames0 of
                        {ok, LoggerNames1} ->
                            LoggerNames2 = lists:usort(LoggerNames1),
                            splice(
                              [emit_dynamic_logger_call(LoggerName, LogLevelVar,
                                                        Args,
                                                        Line, Line1,
                                                        Line2, Line3, Line4) ||
                                  LoggerName <- LoggerNames2]);
                        error ->
                            Stmt
                    end;
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
    case lists:member(LogLevel, ?LOGLEVELS) andalso valid_args(Args) of
        true ->
            case Arg of
                {atom, Line4, LoggerName} ->
                    emit_logger_call(LoggerName, LogLevel, Args,
                                     Line, Line1, Line2, Line3, Line4);
                {cons, Line4, _Head, _Tail} = Cons ->
                    LoggerNames0 =
                        try
                            {ok, ast_atom_list_to_list(Cons)}
                        catch
                            _E:_R ->
                                error
                        end,

                    case LoggerNames0 of
                        {ok, LoggerNames1} ->
                            LoggerNames2 = lists:usort(LoggerNames1),
                            splice([emit_logger_call(LoggerName, LogLevel, Args,
                                                     Line, Line1,
                                                     Line2, Line3, Line4) ||
                                       LoggerName <- LoggerNames2]);
                        error ->
                            Stmt
                    end;
                _Other ->
                    Stmt
            end;
        false ->
            Stmt
    end;
transform(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform(tuple_to_list(Stmt)));
transform(Stmt) when is_list(Stmt) ->
    Transform =
        fun (S) ->
                Expr = transform(S),
                case Expr of
                    {splice, Expr1} -> Expr1;
                    _ -> [Expr]
                end
        end,
    lists:concat([Transform(S) || S <- Stmt]);
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

ast_list_to_list({nil, _Line}) ->
    [];
ast_list_to_list({cons, _Line, Head, Tail}) ->
    [Head | ast_list_to_list(Tail)].

map_ast_list(_Fn, {nil, _Line} = List) ->
    List;
map_ast_list(Fn, {cons, Line, Head, Tail}) ->
    {cons, Line, Fn(Head), map_ast_list(Fn, Tail)}.

ast_atom_to_atom({atom, _Line, Atom}) ->
    Atom.

ast_atom_list_to_list(AstList) ->
    lists:map(fun ast_atom_to_atom/1, ast_list_to_list(AstList)).

splice(Expr) ->
    {splice, Expr}.

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

valid_args(Args) ->
    N = length(Args),
    N =:= 1 orelse N =:= 2.
