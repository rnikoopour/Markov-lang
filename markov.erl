-module(markov).
-export([genTable/2, stop/1, genSentence/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-compile(export_all).
-behaviour(gen_server).
-include_lib("records.hrl").


%% Public
start_link(Name, DbPid) ->
    gen_server:start_link({local, Name}, ?MODULE, factories:dbPidFactory(DbPid), []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

genTable(Pid, String) ->
    gen_server:call(Pid, {genTable, String}).

genSentence(Pid) ->
    gen_server:call(Pid, genSentence).

%% Server API
init(DbController) ->
    {ok, DbController}.

handle_call({genTable, String}, _From, DbController) ->
    TokenList = splitString(String),
    Table = genTable(TokenList, tl(TokenList), tl(tl(TokenList)), []),
    {reply, Table, DbController};
handle_call(genSentence, _From, DbController) ->
    Sentence = privGenSentence(DbController#dbPid.pid),
    {reply, Sentence, DbController};
handle_call(terminate, _From, DbController) ->
    {stop, normal, ok, DbController}.

handle_cast(_, DbController) ->
    {noReply, DbController}.

handle_info(Msg, DbController) ->
    io:format("Not expected: ~p~n", [Msg]),
    {noreply, DbController}.

terminate(normal, _) ->
    ok.

code_change(_OldVsn, DbController, _Extra) ->
    {ok, DbController}.

%% Private API
privGenSentence(DbPid) ->
    genSentence([" ", " "], DbPid).

getSuffixes(Prefix, DbPid) ->
    db_controller:getSuffixes(DbPid, Prefix).

genSentence(["<<<undefined>>>" | T], _) ->
    string:join(lists:reverse(T), " ");
genSentence([Second, First | T], DbPid) ->
    Prefix = First ++ " " ++ Second,
    Suffixes = lists:map(fun ({Suffix, Count}) -> 
				 factories:suffixFactory(Suffix, Count)
			 end, getSuffixes(Prefix, DbPid)),
    ReducedChain = factories:reducedChainFactory(Prefix, Suffixes),
    genSentence([randomSuffix(ReducedChain), Second, First | T], DbPid).


splitString(String) ->
    Tokens = filterPrintableAscii(string:tokens(String, " ")),
    [" ", " "] ++ Tokens ++ ["<<<undefined>>>"].

filterPrintableAscii(TokenList) ->
    lists:map(fun(Token) ->
		     re:replace(Token, "[^ -~]", "", [global, {return, list}])
	      end, TokenList).

genTable(_, _, [], Acc) ->
    reduceTable(Acc);
genTable(L1, L2, L3, Acc) ->
    Prefix = string:join([hd(L1), hd(L2)], " "),
    Suffix = hd(L3),
    Chain = factories:chainFactory(Prefix, Suffix),
    genTable(tl(L1), tl(L2), tl(L3), Acc ++ [Chain]).

hasPrefix(Prefix, #reducedChain{prefix=Prefix,suffixes=_}) ->
    true;
hasPrefix(Prefix, #chain{prefix=Prefix, suffix=_}) ->
    true;
hasPrefix(_,_) ->
    false.

hasSuffix(Suffix, #chain{prefix=_, suffix=Suffix}) ->
    true;
hasSuffix(_,_) ->
    false.

gatherChainsWithPrefix(Prefix, Table) ->
    lists:filter(fun(Chain) ->
			 hasPrefix(Prefix, Chain)
		 end, Table).

gatherChainsWithSuffix(Suffix, Table) ->
    lists:filter(fun(Chain) ->
			hasSuffix(Suffix, Chain)
		 end, Table).

countSameSuffix(Suffix, Chains) ->
    lists:foldl(fun(Chain, NumSame) ->
		 case hasSuffix(Suffix, Chain) of
		    true -> NumSame + 1;
		    false -> NumSame
		 end
	  end, 0, Chains).

genSuffixes([], Acc) ->
    Acc;
genSuffixes(Chains, Acc) ->
    Chain = hd(Chains),
    Suffix = Chain#chain.suffix,
    SameSuffixes = gatherChainsWithSuffix(Suffix, Chains),
    NumSameSuffix = countSameSuffix(Suffix, Chains),
    genSuffixes(Chains -- SameSuffixes, Acc ++ [factories:suffixFactory(Suffix, NumSameSuffix)]).

reduceTable(Table) ->
    reduceTable(Table, []).
reduceTable([], Acc) ->
    Acc;
reduceTable(Table, Acc) ->
    Chain = hd(Table),
    Prefix = Chain#chain.prefix,
    ChainsSamePrefix = gatherChainsWithPrefix(Prefix, Table),
    Suffixes = genSuffixes(ChainsSamePrefix, []),
    reduceTable(Table -- ChainsSamePrefix, Acc ++ [factories:reducedChainFactory(Prefix, Suffixes)]).
    
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
