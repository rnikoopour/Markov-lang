-module(db_controller).
-export([insert/2, getSuffixes/2, verifyDb/1]).
-include("./records.hrl").

%%% Public
verifyDb(DbInfo=#dbInfo{}) ->
    DneMsg = list_to_binary("database \"" ++ DbInfo#dbInfo.db ++ "\" does not exist"),
    case connect(DbInfo#dbInfo.host, DbInfo#dbInfo.user, DbInfo#dbInfo.pass, DbInfo#dbInfo.db) of
	{ok, Connection} ->
	    close(Connection);
	{_, {_, _, _, DneMsg, _}} ->
	    createDB(DbInfo#dbInfo.host, DbInfo#dbInfo.user, DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
	    createTables(DbInfo#dbInfo.host, DbInfo#dbInfo.user, DbInfo#dbInfo.pass, DbInfo#dbInfo.db)
    end,
    ok.

insert(Chains, DbInfo=#dbInfo{}) ->
    {ok, Connection} = connect(DbInfo#dbInfo.host, DbInfo#dbInfo.user,DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
    lists:map(fun (Chain) ->
		      insertChain(Chain, Connection)
	      end, Chains),
    close(Connection).

getSuffixes(Prefix, DbInfo=#dbInfo{}) ->
    {ok, Connection} = connect(DbInfo#dbInfo.host, DbInfo#dbInfo.user,DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
    Suffixes = privGetSuffixes(Prefix, Connection),
    close(Connection),
    Suffixes.

%%% Private
privGetSuffixes(Prefix, Connection) ->
    {ok, _, Suffixes} = epgsql:equery(Connection, "WITH pid AS (SELECT id FROM Prefix WHERE prefixstr=$1) SELECT Suffix.suffixstr, Chain.count FROM Chain INNER JOIN Suffix ON Chain.suffixid=Suffix.id  WHERE prefixid in (SELECT * from pid);", [Prefix]),
    lists:map(fun({SuffixBinStr, Count}) ->
		      {binary_to_list(SuffixBinStr), Count}
	      end, Suffixes).

connect(Host, User, Pass, Db) ->
    epgsql:connect(Host, User, Pass, [{database, Db}]).

close(Connection) ->
    ok = epgsql:close(Connection).

lettersOnly(String) ->
    re:replace(String, "[^a-zA-Z]", "", [global, {return, list}]).

createDB(Host, User, Pass, Db) ->
    {ok, Connection} = epgsql:connect(Host, User, Pass),
    {ok, _, _ } = epgsql:squery(Connection, "CREATE DATABASE " ++ lettersOnly(Db)),
    close(Connection),
    {ok, created}.
    
createTables(Host, User, Pass, Db) ->
    {ok, Connection} = connect(Host, User, Pass, Db),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Prefix (id SERIAL PRIMARY KEY, prefixStr TEXT UNIQUE);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Suffix (id SERIAL PRIMARY KEY, suffixStr TEXT UNIQUE);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Chain (prefixId INTEGER REFERENCES Prefix(id), suffixId INTEGER REFERENCES Suffix(id), count Integer);"),
    ok = close(Connection).

lookUpPrefixId(Prefix, Connection) ->
    {ok, _, Id} = epgsql:equery(Connection, "SELECT id FROM Prefix WHERE prefixStr = $1;", [Prefix]),
    Id.

insertPrefix(Prefix, Connection) ->
    case lookUpPrefixId(Prefix, Connection) of
	[] ->
	    {ok, _, _, [{Id}]} = epgsql:equery(Connection, "INSERT INTO Prefix (prefixStr) values ($1) returning id;", [Prefix]),
	    {prefix, {Prefix, Id}};
	[{Id}] ->
	    {prefix, {Prefix, Id}}
    end.

lookUpSuffixId(Suffix, Connection) ->
    {ok, _, Id} = epgsql:equery(Connection, "SELECT id FROM Suffix WHERE suffixStr = $1;", [Suffix]),
    Id.

insertSuffix(Suffix, Connection) ->
    case lookUpSuffixId(Suffix, Connection) of
	[] ->
	    {ok, _, _, [{Id}]} = epgsql:equery(Connection, "INSERT INTO Suffix (suffixStr) values ($1) returning id;", [Suffix]),
	    {suffix, {Suffix, Id}};
	[{Id}] ->
	    {suffix, {Suffix, Id}}
    end.

lookUpChainCount(PrefixId, SuffixId, Connection) ->
     {ok, _, Count} = epgsql:equery(Connection, "SELECT count FROM Chain WHERE prefixid=$1 AND suffixid=$2", [PrefixId, SuffixId]),
    Count.

updateChains({Prefix, PrefixId}, Suffixes, Connection) ->				     
    lists:map(fun ({Suffix, SuffixId}) ->
		      case lookUpChainCount(PrefixId, SuffixId, Connection) of
			  [] ->
			      {ok, _} = epgsql:equery(Connection, "INSERT INTO Chain VALUES ($1, $2, 1);", [PrefixId, SuffixId]),
			      {Prefix, Suffix, 1};
			  [{Count}] ->
			      {ok, _} = epgsql:equery(Connection, "UPDATE Chain SET count=$1 WHERE prefixid=$2 AND suffixid=$3;", [Count+1, PrefixId, SuffixId]),
			      {Prefix, Suffix, Count+1}
		      end
	      end, Suffixes).

insertChain(Chain=#reducedChain{}, Connection) ->
    {prefix, Prefix} = insertPrefix(Chain#reducedChain.prefix, Connection),
    %% What Suffixes looks like:
    %%  [{Suffix, SuffixId}]
    Suffixes = lists:map(fun (Suffix=#suffix{}) ->
				 {suffix, SuffixInfo} = insertSuffix(Suffix#suffix.word, Connection),
				 SuffixInfo
			 end, Chain#reducedChain.suffixes),
    updateChains(Prefix, Suffixes, Connection).

