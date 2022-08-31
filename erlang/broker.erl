-module(broker).
-export([create_broker/3, broker/3, broker_loop/3]).

% Define a function which takes two list of PIDs [x1, x2, ...], [y1, y2, ...], having the same length, and a
% function f, and creates a different "broker" process for managing the interaction between each pair of
% processes xi and yi.
% At start, the broker process i must send its PID to xi and yi with a message {broker, PID}. Then, the
% broker i will receive messages {from, PID, data, D} from xi or yi, and it must send to the other one an
% analogous message, but with the broker PID and data D modified by applying f to it.
% A special stop message can be sent to a broker i, that will end its activity sending the same message to xi
% and yi.

create_brokers(XS, YS, F) ->
  [spawn(?MODULE, broker, [X, Y, F]) || X <- XS, Y <- YS].

broker(X, Y, F) ->
  X ! {broker, self()},
  Y ! {broker, self()},
  broker_loop(X, Y, F).

broker_loop(X, Y, F) ->
  receive
    {from, PID, data, D} ->
      case PID of
        X ->
          Y ! {from, self(), data, F(D)},
          broker_loop(X, Y, F);
        Y ->
          X ! {from, self(), data, F(D)},
          broker_loop(X, Y, F)
      end;
    stop ->
      X ! {stop},
      Y ! {stop},
      ok
  end.
