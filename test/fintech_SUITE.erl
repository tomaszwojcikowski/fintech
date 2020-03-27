-module(fintech_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, http},
     {group, generic}
    ].

groups() ->
    [
        {generic, [], test_cases()},
        {http, [], http_cases()}
    ].

http_cases() ->
    [ping_test,
    empty_list_test,
    new_transaction_test].

test_cases() ->
    [accounts_loaded,
    transaction_test,
    pending_test,
    account_transaction_test].

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

ping_test(_C) ->
    {ok, Result} = httpc:request("http://localhost:8080/ping"),
    ?assertMatch({{_,200,"OK"},_,"pong"}, Result).

empty_list_test(_C) ->
    {ok, Result} = httpc:request("http://localhost:8080/list"),
    ?assertMatch({{_,200,"OK"},_,"[]"}, Result).

new_transaction_test(_C) ->
    Data = #{from => <<"b">>, to => <<"a">>, amount => 10},
    Body = jiffy:encode(Data),
    Request = {"http://localhost:8080/new", [], "application/json", Body},
    {ok, Result} = httpc:request(post, Request, [], []),
    ?assertMatch({{_,200,"OK"},_, _}, Result),
    {_, _, ResultBody} = Result,
    ResultData = jiffy:decode(ResultBody, [return_maps]),
    ?assertMatch(#{<<"id">> := _}, ResultData),
    ?assertMatch([], transactions:list_pending()).

accounts_loaded(_C) ->
    Accounts = accounts:get_all_accounts(),
    ?assertEqual(3, maps:size(Accounts)).

transaction_test(_) ->
    [] = transactions:list(),
    ?assertEqual(2, get_account_balance(<<"a">>)),
    ?assertEqual(23, get_account_balance(<<"b">>)),
    T = transactions:new(<<"b">>, <<"a">>, 10),
    {ok, Id} = transactions:apply(T),
    Ts = transactions:list(),
    ?assertMatch([#{id := Id, from := <<"b">>, to := <<"a">>, amount := 10}], Ts),
    ?assertEqual(12, get_account_balance(<<"a">>)),
    ?assertEqual(13, get_account_balance(<<"b">>)),
    ok.


account_transaction_test(_) ->
    [] = transactions:list(),
    ?assertEqual(2, get_account_balance(<<"a">>)),
    ?assertEqual(23, get_account_balance(<<"b">>)),
    T = transactions:new(<<"b">>, <<"a">>, 10),
    {ok, Id} = accounts:apply_transaction(<<"b">>, T),
    Ts = transactions:list(),
    ?assertMatch([#{id := Id, from := <<"b">>, to := <<"a">>, amount := 10}], Ts),
    ?assertEqual(12, get_account_balance(<<"a">>)),
    ?assertEqual(13, get_account_balance(<<"b">>)),
    ok.
   
pending_test(_) ->
    ?assertEqual([], transactions:list_pending()),
    T = transactions:new(<<"b">>, <<"a">>, 10),
    {ok, Id} = transactions:add_pending(T),
    ?assertMatch([_], transactions:list_pending()),
    ok = transactions:remove_pending(Id),
    ?assertMatch([], transactions:list_pending()).

% helpers
get_account_balance(Id) ->
    Accounts = accounts:get_all_accounts(),
    maps:get(Id, Accounts).