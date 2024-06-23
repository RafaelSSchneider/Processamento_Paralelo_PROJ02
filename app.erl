-module(app).
-export([start/0, integer_producer/1, float_producer/1, consumer/1, buffer/0, buffer/1, get_time_to_sleep/1]).

start() ->
    io:format("Programa iniciado!~n"),
    BufferPid = spawn(?MODULE, buffer, []),
    create_producers(BufferPid),
    create_consumers(BufferPid).

integer_producer(BufferPid) ->
    io:format("Producer de inteiros ~p produzindo novo item~n", [self()]),
    timer:sleep(3500),
    RandomInt = rand:uniform(256),
    BufferPid ! {produce, RandomInt},
    integer_producer(BufferPid).

float_producer(BufferPid) ->
    io:format("Producer de floats ~p produzindo novo item~n", [self()]),
    timer:sleep(7000),
    RandomFloat = rand:uniform(),
    BufferPid ! {produce, RandomFloat},
    float_producer(BufferPid).

consumer(BufferPid) ->
    BufferPid ! {consume, self()},
    receive
        {item, none} ->
            io:format("Consumer ~p tentou consumir mas o buffer estava vazio~n", [self()]),
            timer:sleep(1000),
            consumer(BufferPid);
        {item, {Type, Value}} ->
            io:format("Item do tipo ~p sendo consumido pelo consumer ~p~n", [Type, self()]),
            TimeToSleep = get_time_to_sleep({Type, Value}),
            timer:sleep(TimeToSleep),
            io:format("Consumer ~p consumiu item ~p~n", [self(), Value]),
            consumer(BufferPid)
    end.

buffer() ->
    buffer([]).

buffer(List) ->
    receive
        {produce, Float} when is_float(Float) ->
            NewList = List ++ [{big, Float}],
            io:format("Buffer recebeu item do tipo Float ~w, novo buffer: ~p~n", [Float, NewList]),
            buffer(NewList);

        {produce, Int} when is_integer(Int) ->
            NewList = List ++ [{small, Int}],
            io:format("Buffer recebeu item do tipo Int ~w, novo buffer: ~p~n", [Int, NewList]),
            buffer(NewList);

        {consume, ConsumerPid} ->
            ConsumerPid ! pause,
            case List of
                [] ->
                    ConsumerPid ! {item, none},
                    buffer(List);
                [Head | Tail] ->
                    ConsumerPid ! {item, Head},
                    buffer(Tail)
            end
    end.

get_time_to_sleep({Type, _Value}) ->
    TimeToSleep = case Type of
        big -> 15000;
        small -> 7000
    end,
    TimeToSleep.

% Cria 2 produtores de tipos diferentes
create_producers(BufferPid) ->
    spawn(?MODULE, integer_producer, [BufferPid]),
    spawn(?MODULE, float_producer, [BufferPid]).

% Cria 4 consumidores
create_consumers(BufferPid) ->
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]).

