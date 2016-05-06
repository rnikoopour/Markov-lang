-module(db_controller).
-behaviour(gen_server).
-compile(export_all).

%%% Public
msg(Pid, get) ->
    gen_server:call(Pid, get);
msg(Pid, Msg) ->
    gen_server:call(Pid, {msg, Msg}).
amsg(Pid, Msg) ->
    gen_server:cast(Pid, {amsg, Msg}).


%%% Private
stop(Pid) ->
    gen_server:call(Pid, terminate).

connect(Host, User, Pass) ->
    epgsql:connect(Host, User, Pass, [{database, "markov"}]).

close(Connection) ->
    ok = epgsql:close(Connection).

createDB(Host, User, Pass) ->
    {ok, Connection} = epgsql:connect(Host, User, Pass),
    {ok, _, _ } = epgsql:squery(Connection, "CREATE DATABASE markov"),
    close(Connection),
    {ok, created}.
    
createTables(Host, User, Pass) ->
    {ok, Connection} = connect(Host, User, Pass),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Prefix (id SERIAL PRIMARY KEY, prefix TEXT);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Suffix (id SERIAL PRIMARY KEY, suffix TEXT);"),
    {ok, [], []} = epgsql:squery(Connection, "CREATE TABLE Chain (prefix INTEGER REFERENCES Prefix(id), suffix INTEGER REFERENCES Suffix(id), count Integer);"),
    created.
verifyDb(Host, User, Pass) ->
    case connect(Host, User, Pass) of
	{ok, Connection} ->
	    close(Connection);
	{_, {_, _, _, <<"database \"markov\" does not exist">>, _}} ->
	    createDB(Host, User, Pass),
	    createTables(Host, User, Pass)
    end,
    ok.

%%% Server
start_link(Host, User, Pass) ->
    gen_server:start_link(?MODULE, {Host, User, Pass}, []).

init({Host, User, Pass}) ->
    verifyDb(Host, User,Pass),
    {ok, {Host, User,Pass}}.

handle_call({insert, _}, _From, DbInfo) ->
    {reply, inserted, DbInfo};
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





