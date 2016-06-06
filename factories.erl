-module(factories).
-export([suffixFactory/2, chainFactory/2, reducedChainFactory/2, dbInfoFactory/4]).
-include("./records.hrl").

suffixFactory(Word, Count) ->
    #suffix{word=Word, count=Count}.
chainFactory(Prefix, Suffix) ->
    #chain{prefix=Prefix, suffix=Suffix}.
reducedChainFactory(Prefix, Suffixes) ->
    #reducedChain{prefix=Prefix, suffixes=Suffixes}.
dbInfoFactory(Username, Pass, DbName, Host) ->
    #dbInfo{host=Host, user=Username, pass=Pass, db=DbName}.
