%%%-------------------------------------------------------------------
%% @doc fintech public API
%% @end
%%%-------------------------------------------------------------------

-module(fintech_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [{"/", fintech_handler, []}]},
            {'_', [{"/new", new_handler, []}]},
            {'_', [{"/list", list_handler, []}]},
            {'_', [{"/pending", pending_handler, []}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
        ),    

    fintech_sup:start_link().

stop(_State) ->
    ok.

%% internal functions