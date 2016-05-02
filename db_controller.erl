-module(db_controller).
-compile(export_all).
-include_lib("records.hrl").

start_db_controller() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, controller, ["localhost", "dhbdhb", ""]),
    register(db, Pid),
    receive
	{'EXIT', Pid, ok} ->
	    ok;
	{'EXIT', Pid, _} ->
	    restarter()
    end.

connect(Hostname, Username, Password) ->
    epgsql:connect(Hostname, Username, Password, [{database, "markov"}]).

createDB(Host, Username, Password) ->
    {ok, Connection} = epgsql:connect(Host, Username, Password),
    {ok, _, _ } = epgsql:squery(Connection, "CREATE DATABASE markov"),
    close(Connection).
    
close(Connection) ->
    ok = epgsql:close(Connection).

verifyDB() ->
    Ref = make_ref(),
    db ! {self(), Ref, verify},
    receive
	{Ref, ok, verified} ->
	    {ok, verified}
    end.

controller(Hostname, Username, Password) ->
    receive
	{From, Ref, verify} ->
	    case connect(Hostname, Username, Password) of
		{ok, Connection} ->
		    close(Connection),
		    From ! {Ref, ok, verified};
		{_, {_, _, _, <<"database \"markov\" does not exist">>, _}} ->
		    createDB(Hostname, Username, Password),
		    From ! {Ref, ok, verified}
	    end
    end,
    controller(Hostname, Username, Password).
