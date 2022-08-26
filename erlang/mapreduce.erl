-module(mapreduce).
-export([start_reduce_mgr/1, reduce_mgr_loop/2, reducer_loop/3, word_count/1]).

% We want to create a simplified implementation of the “Reduce” part of the MapReduce paradigm. To this
% end, define a process “reduce_manager” that keeps track of a pool of reducers. When it is created, it 
% stores a user-defined associative binary function ReduceF. It receives messages of the form {reduce, 
% Key, Value}, and forwards them to a different “reducer” process for each key, which is created lazily 
% (i.e. only when needed). Each reducer serves requests for a unique key.
% Reducers keep into an accumulator variable the result of the application of ReduceF to the values they 
% receive. When they receive a new value, they apply ReduceF to the accumulator and the new value, 
% updating the former. When the reduce_manager receives the message print_results, it makes all its 
% reducers print their key and incremental result.

start_reduce_mgr(ReduceF) ->
    spawn(?MODULE, reduce_mgr_loop, [ReduceF, #{}]).

reduce_mgr_loop(ReduceF, Reducers) ->
    receive
        print_results -> 
            lists:foreach(
                fun({_, P}) -> P ! print_results end,
                maps:to_list(Reducers));
        {reduce, Key, Value} ->
            case Reducers of
                #{Key := Reducer} -> 
                    Reducer ! {reduce, Key, Value},
                    reduce_mgr_loop(ReduceF, Reducers);
                _ -> 
                    Reducer = spawn(?MODULE, reducer_loop, [ReduceF, Key, 0]),
                    Reducer ! {reduce, Key, Value},
                    reduce_mgr_loop(ReduceF, Reducers#{Key => Reducer})
            end
    end.

reducer_loop(ReduceF, Key, Accum) ->
    receive
        print_results ->
            io:format("~s: ~w~n", [Key, Accum]);
        {reduce, Key, Value} -> 
            reducer_loop(ReduceF, Key, ReduceF(Accum, Value))
    end.

word_count(Text) ->
    RMPid = start_reduce_mgr(fun (X, Y) -> X + Y end),
    lists:foreach(fun (Word) -> RMPid ! {reduce, Word, 1} end, string:split(Text, " ", all)),
    RMPid ! print_results,
    ok.
