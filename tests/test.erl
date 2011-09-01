-module(test).

-compile(export_all).
-compile({parse_transform, ale_transform}).

-include("ale.hrl").

prepare() ->
    application:start(ale),

    ok = ale:start_sink(stderr, ale_stderr_sink, []),
    ok = ale:start_sink(disk, ale_disk_sink, ["/tmp/test_log"]),

    ok = ale:add_sink(?ERROR_LOGGER_LOGGER, disk, info),
    ok = ale:add_sink(?ALE_LOGGER, stderr, info),

    ok = ale:start_logger(info),
    ok = ale:start_logger(test),

    ok = ale:add_sink(info, stderr),
    ok = ale:add_sink(info, disk),
    ok = ale:add_sink(test, stderr).

test() ->
    Fn = fun () -> io:format("test local~n") end,
    RemoteSusp = ale:delay(io:format("test remote~n")),

    ale:debug(info,    "test message: ~p", [RemoteSusp]),
    ale:info(info,     "test message: ~p", [Fn()]),
    ale:warn(info,     "test message: ~p", [Fn()]),
    ale:error(info,    "test message: ~p", [RemoteSusp]),
    ale:critical(info, "test message: ~p", [test]),

    ale:xcritical(info, user_data_goes_here,
                  "test message (with user data): ~p", [test]),

    Error = error,
    Info = info,
    GetError = fun () -> error end,
    GetInfo = fun () -> info end,
    ale:log(Info, Error, "dynamic message test: ~p", [test]),
    ale:log(info, Error, "dynamic but known logger: ~p", [test]),
    ale:log(info, GetError(), "dynamic message (fn level): ~p", [test]),
    ale:log(Info, GetError(), "dynamic message (fn level) 2: ~p", [test]),
    ale:log(GetInfo(), GetError(),
            "dynamic message (fn both level and logger: ~p)", [test]),

    ale:xinfo(info, user_data, "test message: ~p", [Fn()]),
    ale:xerror(info, user_data, "test message: ~p", [Fn()]),

    ale:xlog(GetInfo(), error, user_data, "test message: ~p", [test]),
    ale:xlog(info, GetError(), user_data, "test message: ~p", [test]),
    ale:xlog(GetInfo(), GetError(), user_data, "test message: ~p", [test]),

    {error, {badarg, _}} = ale:start_logger(bad_logger, slkdfjlksdj),
    {error, badarg} = ale:set_loglevel(info, lsdkjflsdkj),
    {error, badarg} = ale:set_sync_loglevel(info, lksjdflkjs),
    {error, badarg} = ale:set_sink_loglevel(info, disk, lsdkjflksjd),
    {error, badarg} = ale:add_sink(?ALE_LOGGER, disk, lskdjflksdj),

    ok.

test_perf_loop(0) ->
    ok;
test_perf_loop(Times) ->
    ale:debug(ns_info, "test message: ~p", [test]),
    test_perf_loop(Times - 1).

test_perf() ->
    {Time, _} = timer:tc(fun test_perf_loop/1, [1000000]),
    io:format("Time spent: ~ps~n", [Time div 1000000]).

test_delay() ->
    ale:delay(io:format("=========================delay test: ~p~n", [some_arg])).
