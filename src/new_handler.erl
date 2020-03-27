-module(new_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Data = jiffy:decode(Body, [return_maps]),
    #{<<"from">> := From, <<"to">> := To, <<"amount">> := Amount} = Data,
    T = transactions:new(From, To, Amount),
    {ok, Id} = accounts:apply_transaction(From, T),
    Result = jiffy:encode(#{id => Id}),
   	Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Result, Req),
    {ok, Req1, State}.
