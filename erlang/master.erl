-module(master).
-export([start_master/1, master_loop/2, dummy/0, test/0, initialize_actors/2]).

initialize_actors([], RunningProcesses) -> RunningProcesses;
initialize_actors([X|XS], RunningProcesses) ->
    Child = spawn_link(X),
    initialize_actors(XS, RunningProcesses#{Child => X}).

start_master(L) ->
    process_flag(trap_exit, true),
    RunningProcesses = initialize_actors(L, #{}),
    master_loop(RunningProcesses, length(L)).

master_loop(RP, Count) ->
    receive
        {'EXIT', Child, normal} ->
            io:format("child ~p has ended ~n", [Child]),
            if
                Count =:= 1 -> ok; % the last child has finished executing
                true -> master_loop(RP, Count-1)
            end;
        {'EXIT', Child, _} -> % process has terminated unexpectedly
            #{Child := Func} = RP,
            NewChild = spawn_link(Func),
            master_loop(RP#{NewChild => Func}, Count)
    end.

dummy() ->
    Data = rand:uniform(50),
    if
        Data < 10 -> error("I'm dying");
        Data > 45 -> ok;
        true -> dummy()
    end.

test() ->
    start_master([fun() -> dummy() end, fun() -> dummy() end, fun() -> dummy() end, fun() -> dummy() end, fun() -> dummy() end]).