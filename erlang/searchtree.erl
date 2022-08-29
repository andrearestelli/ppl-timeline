-module(searchtree).
-export([node_wait/3, node_comp_dist/3]).

node_wait(P, C, V) ->
    receive
        {register_child, CPid, W} ->
            node_wait(P, [{CPid, W} | C], V);
        {get_distance, Value, Pid} ->
            if
                Value =:= V -> Pid ! {found, 0};
                true -> node_comp_dist(C, Value, Pid)
            end,
            node_wait(P, C, V)
    end. 

node_comp_dist(C, Value, Pid) ->
    Distances = lists:map(fun({Child, W}) ->
                        Child ! {get_distance, Value, self()},
                        receive
                            {found, Value, Child, D} ->
                                D + W;
                            {not_found, Value, Child} ->
                                -1
                        end
                end, C),
    FilteredDistances = lists:filter(fun(X) -> X >= 0 end, Distances),
    case FilteredDistances of
        [] ->
            Pid ! {not_found, Value, self()};
        _ ->
            Pid ! {found, Value, self(), lists:min(FilteredDistances)}
    end.