-module(assessment4).
-export([logger/0, logger/1, consumer/2, consumerGetData/3, buffer/1, buffer/4, main/0]).

logger() -> logger(0).

logger(Count) ->
    receive Message ->
        io:fwrite("[~w]: ~s~n", [Count, Message]),
        logger(Count + 1)
    end.

consumer(B, Logger) -> consumer(B, Logger, 0).

consumer(B, Logger, Received) ->
    B!{isEmptyQ, self()},
    receive
        empty ->
            Logger!"C: Buffer empty. I wait.",
            receive notEmpty ->
                Logger!"C: Consumer awoke, asking for data",
                consumerGetData(B, Logger, Received)
            end;
        notEmpty ->
            Logger!"C: Asking for data.",
            consumerGetData(B, Logger, Received)
    end.

consumerGetData(B, Logger, Received) ->
    B!{getData, self()},
    receive {data, Msg} -> 
        Logger!io_lib:format("C: Got data: #~w = ~s", [Received, Msg]),
        consumer(B, Logger, Received + 1)
    end.

buffer(MaxSize) ->
    buffer([], MaxSize, none, none).

buffer(BufferData, MaxSize, WaitingConsumer, WaitingProducer) ->
    receive
        {isEmptyQ, C} ->
            case length(BufferData) of
                0 ->
                    C!empty,
                    buffer(BufferData, MaxSize, C, WaitingProducer);
                _ ->
                    C!notEmpty,
                    buffer(BufferData, MaxSize, none, WaitingProducer)
            end;
        {isFullQ, P} ->
            case length(BufferData) of
                MaxSize ->
                    P!full,
                    buffer(BufferData, MaxSize, WaitingConsumer, P);
                _ ->
                    P!notFull,
                    buffer(BufferData, MaxSize, WaitingConsumer, none)
            end;
        {getData, C} ->
            [Data | Datas] = BufferData,
            C!{data, Data},
            case WaitingProducer of
                none -> ok;
                _ -> WaitingProducer!notFull
            end,
            buffer(Datas, MaxSize, WaitingConsumer, none);
        {data, Msg} ->
            case WaitingConsumer of
                none -> ok;
                _ -> WaitingConsumer!notEmpty
            end,
            buffer(BufferData ++ [Msg], MaxSize, none, WaitingProducer)
    end.

main() ->
    Logger = spawn(?MODULE, logger, []),
    Buffer = spawn(?MODULE, buffer, [5]), 
    spawn(?MODULE, consumer, [Buffer, Logger]),
    spawn(producer, producer, [5, Logger, Buffer]).
