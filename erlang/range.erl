-module(range).
-export([range/3, next/1, work/3]).

range(SV, EV, ST) ->
    spawn(?MODULE, work, [SV, EV, ST]).

next(Worker) ->
    Worker ! {ask, self()},
    receive
        {Value} -> Value
    end.

work(V, EV, ST) ->
    receive
        {ask, Pid} -> if
                        V =< EV -> Pid ! {V},
                        work(V+ST, EV, ST);
                        true -> Pid ! {stop_iteration}
                    end
    end.