-module(tripart).
-export([tripart/6, partition/6, work/3]).

% Define a program tripart which takes a list, two values x and y, with x < y, and three functions, taking 
% one argument which must be a list. 
% • tripart first partitions the list in three sublists, one containing values that are less than both x and 
% y, one containing values v such that x ≤ v ≤ y, and one containing values that are greater than both
% x and y.
% • Three processes are then spawned in parallel, running the three given functions and passing the 
% three sublists in order (i.e. the first function must work on the first sublist and so on).
% • Lastly, the program must wait the termination of the three processes in the spawning order, 
% assuming that each one will return the pair {P, V}, where P is its PID and V the resulting value.
% • tripart must return the three resulting values in a list, with the resulting values in the same order 
% as the corresponding sublists.

tripart(L, X, Y, F1, F2, F3) ->
    {LRes, MRes, RRes} = partition(L, X, Y, [], [], []),
    Worker1 = spawn(?MODULE, work, [self(), F1, LRes]),
    Worker2 = spawn(?MODULE, work, [self(), F2, MRes]),
    Worker3 = spawn(?MODULE, work, [self(), F3, RRes]),
    receive
        {Worker1, Value1} ->
            receive
                {Worker2, Value2} ->
                    receive
                        {Worker3, Value3} ->
                            [Value1, Value2, Value3]
                    end
            end
    end.

work(Pid, F, L) ->
    Pid ! {self(), F(L)}.

partition([], _, _, LRes, MRes, RRes) -> 
    {LRes, MRes, RRes};
partition([E|ES], X, Y, LRes, MRes, RRes) ->
    if
        (E < X) -> partition(ES, X, Y, [E | LRes], MRes, RRes);
        (E >= X) and (E =< Y) -> partition(ES, X, Y, LRes, [E | MRes], RRes);
        (E > Y) -> partition(ES, X, Y, LRes, MRes, [E | RRes])
    end.