-module(markovSup).
-behaviour(supervisor).
-export([start_link/3, start_link/5]).
-export([init/1]).
 
start_link(Name, DbUsername, DbName) ->
    start_link(Name, DbUsername, "", DbName, "localhost").
start_link(Name, DbUsername, DbPass, DbName, DbHost) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Name, DbUsername, DbPass, DbName, DbHost]).

init(Params) ->
    {ok, {{one_for_one, 5, 60},
     [{markov, 
       {markovHandler, start_link, Params},
       permanent, 5000, worker, [markovHandler]}]}}.
