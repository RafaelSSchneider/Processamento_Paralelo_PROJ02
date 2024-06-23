-module(app).
-export([start/0, number_producer/1, string_producer/1, consumer/1, buffer/0, buffer/1, get_time_to_sleep/1]).

start() ->
    io:format("Programa iniciado!~n"),
    BufferPid = spawn(?MODULE, buffer, []),
    create_producers(BufferPid),
    create_consumers(BufferPid).

number_producer(BufferPid) ->
    io:format("Producer de numeros ~p produzindo novo item~n", [self()]),
    timer:sleep(3500),
    RandomNum = rand:uniform(256),
    BufferPid ! {produce, RandomNum},
    number_producer(BufferPid).

string_producer(BufferPid) ->
    io:format("Producer de strings ~p produzindo novo item~n", [self()]),
    timer:sleep(7000),
    RandomString = generate_random_string(),
    BufferPid ! {produce, RandomString},
    string_producer(BufferPid).

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
        {produce, Str} when is_list(Str) ->
            NewList = List ++ [{big, Str}],
            io:format("Buffer recebeu item do tipo str ~s, novo buffer: ~p~n", [Str, NewList]),
            buffer(NewList);

        {produce, Num} when is_integer(Num) ->
            NewList = List ++ [{small, Num}],
            io:format("Buffer recebeu item do tipo num ~w, novo buffer: ~p~n", [Num, NewList]),
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
    spawn(?MODULE, number_producer, [BufferPid]),
    spawn(?MODULE, string_producer, [BufferPid]).

% Cria 4 consumidores
create_consumers(BufferPid) ->
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]).

generate_random_string() ->
    Integer = rand:uniform(26),  % Números de 1 a 26 (para letras de A a Z)
    integer_to_list(64 + Integer).

