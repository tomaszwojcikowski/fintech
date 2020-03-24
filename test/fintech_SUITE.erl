-module(fintech_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fintech),
    Config.

end_per_suite(_Config) ->
    application:stop(fintech),
    ok.