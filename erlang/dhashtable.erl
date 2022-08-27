-module(dhashtable).
-export([hashtable_spawn/2, hashtable_loop/2, spawn_buckets/2, bucket_loop/1, main/0]).

% Create a distributed hash table with separate chaining. The hash table will consist of an agent for each
% bucket, and a master agent that stores the buckets’ PIDs and acts as a middleware between them and the
% user. Actual key/value pairs are stored into the bucket agents.
% The middleware agent must be implemented by a function called hashtable_spawn that takes as its
% arguments (1) the hash function and (2) the number of buckets. When executed, hashtable_spawn
% spawns the bucket nodes, and starts listening for queries from the user. Such queries can be of two kinds:
% • Insert: {insert, Key, Value} inserts a new element into the hash table, or updates it if an
% element with the same key exists;
% • Lookup: {lookup, Key, RecipientPid} sends to the agent with PID “RecipientPid” a
% message of the form {found, Value}, where Value is the value associated with the given key, if
% any. If no such value exists, it sends the message not_found.

hashtable_spawn(HashF, N) ->
    Buckets = spawn_buckets(N, #{}),
    hashtable_loop(HashF, Buckets).

hashtable_loop(HashF, Buckets) ->
    receive
        {insert, Key, Value} ->
            KeyHashed = HashF(Key),
            #{KeyHashed := Bucket} = Buckets,
            Bucket ! {insert, Key, Value},
            hashtable_loop(HashF, Buckets);
        {lookup, Key, RecipientPid} ->
            KeyHashed = HashF(Key),
            #{KeyHashed := Bucket} = Buckets,
            Bucket ! {lookup, Key, RecipientPid},
            hashtable_loop(HashF, Buckets)
    end.

spawn_buckets(0, Buckets) ->
    Buckets;
spawn_buckets(N, Buckets) ->
    Bucket = spawn(?MODULE, bucket_loop, [#{}]),
    spawn_buckets(N - 1, Buckets#{N - 1 => Bucket}).

bucket_loop(HashTable) ->
    receive
        {insert, Key, Value} ->
            bucket_loop(HashTable#{Key => Value});
        {lookup, Key, RecipientPid} ->
            case HashTable of
                #{Key := Val} ->
                    RecipientPid ! {found, Val};
                _ ->
                    RecipientPid ! {not_found}
            end,
            bucket_loop(HashTable)
    end.

main() ->
    HT = spawn(?MODULE, hashtable_spawn, [fun(Key) -> Key rem 7 end, 7]),
    HT ! {insert, 15, "Apple"},
    HT ! {insert, 8, "Orange"},
    timer:sleep(500),
    HT ! {lookup, 8, self()},
    receive
        {found, A1} -> io:format("~s~n", [A1])
    end,
    HT ! {insert, 8, "Pineapple"},
    timer:sleep(500),
    HT ! {lookup, 8, self()},
    receive
        {found, A2} -> io:format("~s~n", [A2])
    end.
