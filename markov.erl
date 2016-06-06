-module(markov).
-include_lib("records.hrl").
-compile(export_all).

splitString(String) ->
    Tokens = filterPrintableAscii(string:tokens(String, " ")),
    [" ", " "] ++ Tokens ++ ["<<<undefined>>>"].

filterPrintableAscii(TokenList) ->
    lists:map(fun(Token) ->
		     re:replace(Token, "[^ -~]", "", [global, {return, list}])
	      end, TokenList).

genTable(String) ->
    TokenList = splitString(String),
    genTable(TokenList, tl(TokenList), tl(tl(TokenList)), []).
genTable(_, _, [], Acc) ->
    reduceTable(Acc);
genTable(L1, L2, L3, Acc) ->
    Prefix = string:join([hd(L1), hd(L2)], " "),
    Suffix = hd(L3),
    Chain = factories:chainFactory(Prefix, Suffix),
    genTable(tl(L1), tl(L2), tl(L3), Acc ++ [Chain]).

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

genSuffixes(Chains) ->
    genSuffixes(Chains, []).
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
    Suffixes = genSuffixes(ChainsSamePrefix),
    reduceTable(Table -- ChainsSamePrefix, Acc ++ [factories:reducedChainFactory(Prefix, Suffixes)]).
    
