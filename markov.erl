-module(markov).
-export([genTable/1]).
-record(chain, {prefix="", suffix=""}).
-record(suffix, {word="", count=0}).
-record(reducedChain, {prefix="", suffixes=[]}).

suffixFactory(Word, Count) ->
    #suffix{word=Word, count=Count}.
chainFactory(Prefix, Suffix) ->
    #chain{prefix=Prefix, suffix=Suffix}.
reducedChainFactory(Prefix, Suffixes) ->
    #reducedChain{prefix=Prefix, suffixes=Suffixes}.

splitString(String) ->
    ["", ""] ++ string:tokens(String, " ").

genTable(String) ->
    TokenList = splitString(String),
    genTable(TokenList, tl(TokenList), tl(tl(TokenList)), []).
genTable(_, _, [], Acc) ->
    reduceTable(Acc);
genTable(L1, L2, L3, Acc) ->
    Prefix = string:join([hd(L1), hd(L2)], " "),
    Suffix = hd(L3),
    Chain = chainFactory(Prefix, Suffix),
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
    AccSuffix = suffixFactory(Suffix, NumSameSuffix),
    genSuffixes(Chains -- SameSuffixes, Acc ++ [AccSuffix]).

reduceTable(Table) ->
    reduceTable(Table, []).
reduceTable([], Acc) ->
    Acc;
reduceTable(Table, Acc) ->
    Chain = hd(Table),
    Prefix = Chain#chain.prefix,
    ChainsSamePrefix = gatherChainsWithPrefix(Prefix, Table),
    Suffixes = genSuffixes(ChainsSamePrefix, []),
    reduceTable(Table -- ChainsSamePrefix, Acc ++ [reducedChainFactory(Prefix, Suffixes)]).
    
    
