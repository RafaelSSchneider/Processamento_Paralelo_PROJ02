-module(app). %define o nome do modulo que será usado
-export([start/0]). %permite chamar as funções por fora do arquivo

% create_item(IsReady, IsLocked, Time) ->
%     {{IsReady}, {IsLocked}, {Time}}.

start() ->
    io:format("Started program!~n"),
    MaxItems = 6,
    Items = [], 
    Producer_list = create_producer(),
    Consumer_list = create_consumer(),
    print_producer_list(Producer_list),
    print_consumer_list(Consumer_list).

producer() ->
    io:format("Producer started~n"),
    receive
        stop -> io:format("Producer stopped~n");
        _ -> 
            io:format("Producer is trying to create a item!~n"),
            timer:sleep(1000),
            producer()
    end.

consumer() ->
    io:format("Consumer started~n"),
    receive
    stop -> io:format("Consumer stopped~n");
    _ -> 
        io:format("Consumer is trying to eat a item!~n"),
        timer:sleep(1000),
        producer()
    end.


create_producer() ->
    Producer_list = [spawn(fun() -> producer() end) || _ <- lists:seq(1, 2)], %cria 2 produtores
    Producer_list.

create_consumer() ->
    Consumer_list = [spawn(fun() -> consumer() end) || _ <- lists:seq(1, 4)], %cria 4 consumidores
    Consumer_list.

print_producer_list(Producer_list) ->
    io:format("Producer list: ~p~n", [Producer_list]).

print_consumer_list(Consumer_list) ->
    io:format("Consumer list: ~p~n", [Consumer_list]).