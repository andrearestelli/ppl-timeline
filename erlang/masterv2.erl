-module(masterv2).
-export([main/2, spawn_children/3, master_loop/2]).

% Consider a main process which takes two lists: one of function names, and one of lists of parameters (the 
% first element of with contains the parameters for the first function, and so forth). For each function, the 
% main process must spawn a worker process, passing to it the corresponding argument list. If one of the 
% workers fails for some reason, the main process must create another worker running the same function. 
% The main process ends when all the workers are done.

main(FL, PL) ->
    process_flag(trap_exit, true),
    Workers = spawn_children(FL, PL, #{}),
    master_loop(length(maps:tolist(Workers)), Workers).

spawn_children([], [], Workers) -> Workers;
spawn_children([F|FS], [P|PS], Workers) ->
    ChildPid = spawn_link(?MODULE, F, [P]),
    spawn_children(FS, PS, Workers#{ChildPid => {F, P}}).

master_loop(Count, Workers) ->
    receive
        {'EXIT', _, normal} ->
            if
                Count =:= 1 -> ok;
                true -> master_loop(Count-1, Workers)
            end;
        {'EXIT', Child, _} ->
            #{Child := {F, P}} = Workers,
            NewChild = spawn_link(?MODULE, F, [P]),
            master_loop(Count, Workers#{NewChild => {F, P}})
    end.