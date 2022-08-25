-module(fixedpoint).
-export([fix/2, applyf/2, test/1, main_loop/2]).

% The fixed-point of a function f and a starting value x is the value v = fk(x), with k > 0, such that fk(x) = fk+1(x). We want to 
% implement a fixed-point code using two communicating actors:
% 1) Define the function for an applier actor, which has a state S, holding a value, and receives a function f from other actors: if S = 
% f(S), it sends back the result S and ends it computation; otherwise sends back a message to state that the condition S = f(S) has not 
% been reached. 
% 2) Define a function called fix, which takes as input a function and a starting value, and creates and uses an applier actor to 
% implement the fixed-point.

fix(F, SV) ->
    Applier = spawn(?MODULE, applyf, [self(), SV]),
    Applier ! {F},
    main_loop(Applier, F).

main_loop(Applier, F) ->
    receive
        {continue} -> 
            Applier ! {F},
            main_loop(Applier, F);
        {ended, RES} -> RES
    end.

applyf(Pid, V) ->
    receive
        {F} ->  RES = F(V),
                if
                    RES =:= V -> Pid ! {ended, RES};
                    true -> Pid ! {continue},
                            applyf(Pid, RES)
                end
    end.

test(SV) -> 
    fix(fun(X) -> 
            if
                X >= 5 -> 5;
                true -> X + 1
            end
        end, SV).