-module(fintech_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, generic}
    ].

groups() ->
    [{generic, [], test_cases()}
    ].

test_cases() ->
    [accounts_loaded].

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fintech),
    Config.

end_per_suite(_Config) ->
    application:stop(fintech),
    ok.

init_per_testcase(_, Config) ->
    ok = fintech_rdbms:query(<<"TRUNCATE `accounts`">>),
    ok = fintech_rdbms:query(<<"TRUNCATE `transactions`">>),
    fintech_app:load_accounts(),
    Config.

% TEST CASES

accounts_loaded(_C) ->
    Accounts = accounts:get_all_accounts(),
    3 = length(Accounts).