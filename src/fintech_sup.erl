%%%-------------------------------------------------------------------
%% @doc fintech top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fintech_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => accounts_sup,
          start => {accounts_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
