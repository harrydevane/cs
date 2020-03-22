-module(assessment3).
-export([relay/0, sender/2, receiver/1, spawn/0, computeA/4, computeB/4, testDConcurrent/3, test/0]).

relay() ->
    receive {iamSender, P} ->
        receive {iamReceiver, Q} ->
            receive {X, P} ->
                Q!(X + 1),
                relay()
            end
        end
    end.

sender(Relay, X) ->
    Relay!{iamSender, self()},
    Relay!{X, self()}.

receiver(Relay) ->
    Relay!{iamReceiver, self()},
    receive Data -> io:fwrite("Got ~w~n", [Data]) end.

spawn() ->
    X = 20,
    Relay = spawn(?MODULE, relay, []),
    spawn(?MODULE, sender, [Relay, X]),
    spawn(?MODULE, receiver, [Relay]).

computeA(F, X, Z, Pid) -> Pid!({self(), F(X, Z)}).

computeB(F, Y, W, Pid) -> Pid!({self(), F(Y, W)}).

testDConcurrent(F, {X, Y}, {Z, W}) ->
    ComputeA = spawn(?MODULE, computeA, [F, X, Z, self()]),
    ComputeB = spawn(?MODULE, computeB, [F, Y, W, self()]),
    receive {ComputeA, A} ->
        receive {ComputeB, B} -> {A, B} end
    end.

test() -> 
    F = fun (A, B) -> A + B end,
    X = 20,
    Y = 30,
    Z = 40,
    W = 50,
    testDConcurrent(F, {X, Y}, {Z, W}).
