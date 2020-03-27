-module(new_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Data = jiffy:decode(Body, [return_maps]),
    #{<<"from">> := From, <<"to">> := To, <<"amount">> := Amount} = Data,
    T = transactions:new(From, To, Amount),
    {Code, Result} = handle_new(T),
   	Req1 = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Result), 
        Req),
    {ok, Req1, State}.


handle_new(T) ->
    % todo validation
    case accounts:apply_transaction(T) of
        {ok, Id} ->
            {200, #{id => Id}};
        {error, timeout} ->
            {408, #{timeout => true}};
        {error, executing} ->
            retry(T)
    end.

retry(T) ->
    Interval = timer:uniform(500),
    timer:sleep(Interval),
    handle_new(T).