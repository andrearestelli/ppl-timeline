-module(pfilter).
-export([filter/2, runit/3]).

% Define a parallel implementation of the classical functional higher order function filter.
runit(Proc, Pred, X) -> case Pred(X) of
                            true -> Proc ! {self(), [X]};
                            false -> Proc ! {self(), []}
                        end.

filter(Pred, L) -> 
    W = lists:map(fun(X) -> 
                            (spawn(?MODULE, runit, [self(), Pred, X]))
                  end, L),
    lists:foldr(fun(P,Res) -> 
                    receive
                        {P, V} -> V ++ Res
                    end
                end, [], W).