-module(splitmap).
-export([prefix/2, suffix/2, split/2, pmap/3, splitmap/3, splitmap_loop/3]).

% 1) Define a split function, which takes a list and a number n and returns a pair of lists, where the first one 
% is the prefix of the given list, and the second one is the suffix of the list of length n.
% E.g. split([1,2,3,4,5], 2) is {[1,2,3],[4,5]}.

prefix(0, _) -> [];
prefix(N, [X|XS]) ->
    [X] ++ prefix(N-1, XS).

suffix(N, [X|XS]) ->
    if
        length(XS) + 1 =:= N -> [X|XS];
        true -> suffix(N, XS)
    end.

split(L, N) ->
    {prefix(length(L) - N, L), suffix(N, L)}.

% 2) Using split of 1), define a splitmap function which takes a function f, a list L, and a value n, and splits 
% L with parameter n, then launches two process to map f on each one of the two lists resulting from the 
% split. The function splitmap must return a pair with the two mapped lists.

pmap(Pid, F, L) ->
    RES = lists:map(F, L),
    Pid ! {self(), RES}.

splitmap(F, L, N) ->
    {PREF, SUF} = split(L, N),
    PrefWorker = spawn(?MODULE, pmap, [self(), F, PREF]),
    SufWorker = spawn(?MODULE, pmap, [self(), F, SUF]),
    splitmap_loop(PrefWorker, SufWorker, false).

splitmap_loop(PrefWorker, SufWorker, Ready) ->
    receive
        {PrefWorker, PrefRes} -> case Ready of
                                    true -> {PrefRes, SufWorker};
                                    false -> splitmap_loop(PrefRes, SufWorker, true)
                                end;
        {SufWorker, SufRes} -> case Ready of
                                    true -> {PrefWorker, SufRes};
                                    false -> splitmap_loop(PrefWorker, SufRes, true)
                                end
    end.