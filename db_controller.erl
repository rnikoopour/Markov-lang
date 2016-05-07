-module(db_controller).
-behaviour(gen_server).
-export([start_link/4, insert/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).
-include("./records.hrl").
%%% Public
start_link(Host, User, Pass, Db) ->
    gen_server:start_link(?MODULE, #dbInfo{host=Host, user=User, pass=Pass, db=Db}, []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

insert(Pid, Msg) ->
    gen_server:call(Pid, {insert, Msg}).

%%% Private
connect(Host, User, Pass, Db) ->
    epgsql:connect(Host, User, Pass, [{database, Db}]).

close(Connection) ->
    ok = epgsql:close(Connection).

createDB(Host, User, Pass, Db) ->
    {ok, Connection} = epgsql:connect(Host, User, Pass),
    {ok, _, _ } = epgsql:squery(Connection, "CREATE DATABASE " ++ Db),
    close(Connection),
    {ok, created}.
    
createTables(Host, User, Pass, Db) ->
    {ok, Connection} = connect(Host, User, Pass, Db),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Prefix (id SERIAL PRIMARY KEY, prefix TEXT);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Suffix (id SERIAL PRIMARY KEY, suffix TEXT);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Chain (prefix INTEGER REFERENCES Prefix(id), suffix INTEGER REFERENCES Suffix(id), count Integer);"),
    created.

verifyDb(Host, User, Pass, Db) ->
    DneMsg = list_to_binary("database \"" ++ Db ++ "\" does not exist"),
    io:format("~p ~n", [DneMsg]),
    case connect(Host, User, Pass, Db) of
	{ok, Connection} ->
	    close(Connection);
	{_, {_, _, _, DneMsg, _}} ->
	    createDB(Host, User, Pass, Db),
	    createTables(Host, User, Pass, Db)
    end,
    ok.

%%% Server
init(DbInfo=#dbInfo{}) ->
    verifyDb(DbInfo#dbInfo.host, DbInfo#dbInfo.user,DbInfo#dbInfo.pass, DbInfo#dbInfo.db),
    {ok, DbInfo}.

handle_call({insert, M}, _From, DbInfo) ->
    {reply, {inserted, M}, DbInfo};
handle_call(terminate, _From, DbInfo) ->
    {stop, normal, ok, DbInfo};
handle_call(get, _From, DbInfo) ->
    {reply, DbInfo, DbInfo}.

handle_cast(_, DbInfo) ->
    {noreply, DbInfo}.

handle_info(Msg, DbInfo) ->
    io:format("Not expected: ~p~n", [Msg]),
    {noreply, DbInfo}.

terminate(normal, _) ->    
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

