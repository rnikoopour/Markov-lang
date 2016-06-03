-module(db_controller).
-behaviour(gen_server).
-export([start_link/2, start_link/4, insert/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).
-include("./records.hrl").

%%% Public
start_link(User, Db) ->
    start_link("localhost", User, "", Db).
start_link(Host, User, Pass, Db) ->
    gen_server:start_link({local, list_to_atom(Db)}, ?MODULE, #dbInfo{host=Host, user=User, pass=Pass, db=Db}, []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

insert(Pid, Msg) ->
    gen_server:call(Pid, {insert, Msg}).

%%% Private
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

verifyDb(Host, User, Pass, Db) ->
    DneMsg = list_to_binary("database \"" ++ Db ++ "\" does not exist"),
    case connect(Host, User, Pass, Db) of
	{ok, Connection} ->
	    close(Connection);
	{_, {_, _, _, DneMsg, _}} ->
	    createDB(Host, User, Pass, Db),
	    createTables(Host, User, Pass, Db)
    end,
    ok.

lookUpPrefix(Prefix, Connection) ->
    {ok, _, Id} = epgsql:equery(Connection, "SELECT id FROM Prefix WHERE prefixStr = $1;", [Prefix]),
    Id.

insertPrefix(Prefix, Connection) ->
    case lookUpPrefix(Prefix, Connection) of
	[] ->
	    {ok, _, _, [{Id}]} = epgsql:equery(Connection, "INSERT INTO Prefix (prefixStr) values ($1) returning id;", [Prefix]),
	    {prefix, {Prefix, Id}};
	[{Id}] ->
	    {prefix, {Prefix, Id}}
    end.

lookUpSuffix(Suffix, Connection) ->
    {ok, _, Id} = epgsql:equery(Connection, "SELECT id FROM Suffix WHERE suffixStr = $1;", [Suffix]),
    Id.

insertSuffix(Suffix, Connection) ->
    case lookUpSuffix(Suffix, Connection) of
	[] ->
	    {ok, _, _, [{Id}]} = epgsql:equery(Connection, "INSERT INTO Suffix (suffixStr) values ($1) returning id;", [Suffix]),
	    {suffix, {Suffix, Id}};
	[{Id}] ->
	    {suffix, {Suffix, Id}}
    end.

lookUpChain(PrefixId, SuffixId, Connection) ->
     {ok, _, Count} = epgsql:equery(Connection, "SELECT count FROM Chain WHERE prefixid=$1 AND suffixid=$2", [PrefixId, SuffixId]),
    Count.

updateChains({Prefix, PrefixId}, Suffixes, Connection) ->				     
    lists:map(fun ({Suffix, SuffixId}) ->
		      case lookUpChain(PrefixId, SuffixId, Connection) of
			  [] ->
			      {ok, _} = epgsql:equery(Connection, "INSERT INTO Chain VALUES ($1, $2, 1)", [PrefixId, SuffixId]),
			      {Prefix, Suffix, 1};
			  [{Count}] ->
			      {ok, _} = epgsql:equery(Connection, "UPDATE Chain SET count=$1 WHERE prefixid=$2 AND suffixid=$3", [Count+1, PrefixId, SuffixId]),
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

%%% Server
init(DbInfo=#dbInfo{}) ->
    verifyDb(DbInfo#dbInfo.host, DbInfo#dbInfo.user,DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
    {ok, DbInfo}.

handle_call({insert, Chains}, _From, DbInfo) ->
    {ok, Connection} = connect(DbInfo#dbInfo.host, DbInfo#dbInfo.user,DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
    Reply = lists:map(fun (Chain) ->
			      insertChain(Chain, Connection)
		      end, Chains),
    close(Connection),
    {reply, Reply, DbInfo};
handle_call(terminate, _From, DbInfo) ->
    {stop, normal, ok, DbInfo}.

handle_cast(_, DbInfo) ->
    {noreply, DbInfo}.

handle_info(Msg, DbInfo) ->
    io:format("Not expected: ~p~n", [Msg]),
    {noreply, DbInfo}.

terminate(normal, _) ->    
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

