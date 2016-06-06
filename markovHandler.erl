-module(markovHandler).
-export([start_link/3, start_link/5, genTable/2, stop/1, genSentence/1, remember/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-behaviour(gen_server).
-include_lib("records.hrl").


%% Public
start_link(Name, DbUsername, DbName) ->
    start_link(Name, DbUsername, "", DbName, "localhost").
start_link(Name, DbUsername, DbPass, DbName, DbHost) ->
    gen_server:start_link({local, Name}, ?MODULE, factories:dbInfoFactory(DbUsername, DbPass, DbName, DbHost), []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

genTable(Pid, String) ->
    gen_server:call(Pid, {genTable, String}).

remember(Pid, String) ->
    gen_server:cast(Pid, {remember, String}).

genSentence(Pid) ->
    gen_server:call(Pid, genSentence).

%% Server API
init(DbInfo=#dbInfo{}) ->
    db_controller:verifyDb(DbInfo),
    {ok, DbInfo}.

handle_call({genTable, String}, _From, DbInfo) ->
    {reply, genTable(String), DbInfo};
handle_call(genSentence, _From, DbInfo=#dbInfo{}) ->
    {reply, privGenSentence(DbInfo), DbInfo};
handle_call(terminate, _From, DbController) ->
    {stop, normal, ok, DbController}.

handle_cast({remember, String}, DbInfo=#dbInfo{}) ->
    Table = genTable(String),
    db_controller:insert(Table, DbInfo),
    {noreply, DbInfo};
handle_cast(_, DbInfo) ->
    {noreply, DbInfo}.

handle_info(Msg, DbController) ->
    io:format("Not expected: ~p~n", [Msg]),
    {noreply, DbController}.

terminate(normal, _) ->
    ok.

code_change(_OldVsn, DbController, _Extra) ->
    {ok, DbController}.

%% Private API
genTable(String) ->
    markov:genTable(String).    

privGenSentence(DbInfo=#dbInfo{}) ->
    genSentence([" ", " "], DbInfo).

genSentence(["<<<undefined>>>" | T], _) ->
    % double tail removes blank spaces from front
    string:join(tl(tl(lists:reverse(T))), " ");
genSentence([Second, First | T], DbInfo=#dbInfo{}) ->
    Prefix = First ++ " " ++ Second,
    Suffixes = lists:map(fun ({Suffix, Count}) -> 
				 factories:suffixFactory(Suffix, Count)
			 end,  db_controller:getSuffixes(Prefix, DbInfo)),
    ReducedChain = factories:reducedChainFactory(Prefix, Suffixes),
    genSentence([randomSuffix(ReducedChain), Second, First | T], DbInfo).

randomSuffix(#reducedChain{prefix=_, suffixes=Suffixes}) ->
    TotalWeight = lists:foldl(fun(#suffix{word=_, count=Count}, Sum) ->
				   Sum + Count
			    end, 0, Suffixes),
    Trigger = random:uniform() * TotalWeight,
    try 
	lists:foldl(fun(#suffix{word=Suffix, count=Count}, TriggerValue) ->
			    Selected = TriggerValue - Count,
			    case Selected =< 0 of
				true -> throw({selection, Suffix});
				false -> Selected
			    end
		    end, Trigger, Suffixes)
	catch
	    {selection, Choice} -> Choice
     end.
