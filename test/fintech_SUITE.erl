-module(fintech_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, generic}
    ].

groups() ->
    [{generic, [], test_cases()}
    ].

test_cases() ->
    [accounts_loaded,
    transaction_test,
    pending_test].

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
    ?assertEqual(3, maps:size(Accounts)).

transaction_test(_) ->
    [] = transactions:list(),
    ?assertEqual(2, get_account_balance(<<"a">>)),
    ?assertEqual(23, get_account_balance(<<"b">>)),
    {ok, Id} = transactions:apply(<<"b">>, <<"a">>, 10),
    T = transactions:list(),
    ?assertMatch([[Id, <<"b">>, <<"a">>, 10|_]], T),
    ?assertEqual(12, get_account_balance(<<"a">>)),
    ?assertEqual(13, get_account_balance(<<"b">>)),
    ok.
   
pending_test(_) ->
    ?assertEqual([], transactions:list_pending()),
    {ok, Id} = transactions:add_pending(<<"a">>, <<"b">>, 10),
    ?assertMatch([_], transactions:list_pending()),
    ok = transactions:remove_pending(Id),
    ?assertMatch([], transactions:list_pending()).

% helpers
get_account_balance(Id) ->
    Accounts = accounts:get_all_accounts(),
    maps:get(Id, Accounts).