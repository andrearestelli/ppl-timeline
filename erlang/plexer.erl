-module(plexer).
-export([split/2, plex/2, lex/3, atomize/2]).

% Define a parallel lexer, which takes as input a string x and a chunk size n, and translates all the words in 
% the strings to atoms, sending to each worker a chunk of x of size n (the last chunk could be shorter than 
% n). You can assume that the words in the string are separated only by space characters (they can be more 
% than one - the ASCII code for ' ' is 32); it is ok also to split words, if they overlap on different chunks.
% e.g. plex("this is a nice test", 6) returns [[this,i],[s,a,ni],[ce,te],[st]]


plex(String, N) ->
    Tokens = split(String, N),
    Workers = lists:map(fun(X) ->
            spawn(?MODULE, atomize, [self(), X])
            end   
            ,Tokens),
    lists:map(fun(X) ->
                receive
                    {X, RES} -> RES
                end
            end 
            , Workers).

atomize(Pid, String) ->
    Atoms = lex(String, [], []),
    Pid ! {self(), Atoms}.

split(String, N) when length(String) < N -> [String];
split(String, N) ->
    [lists:sublist(String, 1, N) | split(lists:sublist(String, N+1, (length(String) - N)), N)].

lex([], [], Result) -> Result;
lex([], Word, Result) -> Result ++ [list_to_atom(Word)];
lex([X|XS], Word, Result) when X =:= 32 ->
    lex(XS, [], Result ++ [list_to_atom(Word)]);
lex([X|XS], Word, Result) ->
    lex(XS, Word ++ [X], Result).