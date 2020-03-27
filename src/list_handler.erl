-module(list_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    List = transactions:list(),
    Json = jiffy:encode(List),
   	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req0),
    {ok, Req, State}.
