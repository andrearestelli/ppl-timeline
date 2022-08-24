-module(activatetree).
-export([activate/2, leafy/1, branchy/5, test/0]).

% Consider a binary tree stored with tuples, e.g. {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}}.
% Define an activate function, which takes a binary tree and a binary function f and creates a network of actors having the same
% structure of the given tree. Actors corresponding to leaves run a function called leafy, that must be defined, which answer to the
% message {ask, P} by sending to process P the pair {self(), V}, where V is the value stored in the leaf, then terminate.
% Actors for the branches run a function called branchy, that must be also defined: if they receive an {ask, P} request like that of
% leaves, they ask both their sons; when they receive the answers, they call f on the obtained values, then send the result V to P as
% {self(), V} and terminate.

activate({branch, L, R}, F) ->
    LeftPID = activate(L, F),
    RightPID = activate(R, F),
    spawn(?MODULE, branchy, [LeftPID, RightPID, F, false, none]);
activate({leaf, Value}, _) ->
    spawn(?MODULE, leafy, [Value]).

branchy(L, R, F, Ready, Parent) ->
    receive
        {ask, P} ->
            L ! {ask, self()},
            R ! {ask, self()},
            branchy(L, R, F, Ready, P);
        {L, VL} -> case Ready of 
                        true -> Parent ! {self(), F(VL, R)};
                        false -> branchy(VL, R, F, true, Parent)
                    end;
        {R, VR} -> case Ready of
                        true -> Parent ! {self(), F(L, VR)};
                        false -> branchy(L, VR, F, true, Parent)
                    end
    end.

leafy(V) ->
    receive
        {ask, P} -> P ! {self(), V}
    end.

test() ->
    T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
    A1 = activate(T1, fun min/2),
    A1 ! {ask, self()},
    receive
        {A1, V} -> V
    end.
