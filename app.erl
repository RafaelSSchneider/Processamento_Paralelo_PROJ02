%precisamos arrumar a forma de deletar o item da lista de itens
%enviar a lista atualizada através de mensagens para os consumidores
% consumidores precisam enviar mensagem aos produtoes disponiveis
% podemos adicionar um receive nos consumidores esperando um produtor
% avisar que um item novo foi criado e esta pronto para ser consumido

-module(app). %define o nome do modulo que será usado
-export([start/0]). %permite chamar as funções por fora do arquivo

start() ->
    io:format("Started program!~n"),
    MaxItems = 6,
    Items = [], 
    Producer_list = create_producer(Items, MaxItems),
    Consumer_list = create_consumer(Items, Producer_list),
    print_producer_list(Producer_list),
    print_consumer_list(Consumer_list),
    print_item_list(Items).

-record(item, {id, name}).

producer(Items, MaxItems) ->
    if 
        length(Items) =< MaxItems -> 
            timer:sleep(3500),
            %NewItem = #item{name = "Item", id = erlang:unique_integer([positive])},
            io:format("Item created!~n"),
            producer([NewItem | Items], MaxItems);
        length(Items) >= MaxItems -> 
            receive
                {consumer, _} -> 
                    io:format("Item consumed!~n"),
                    producer(Items, MaxItems)
            end
    end,
    timer:sleep(1000),
    producer(Items, MaxItems).


consumer(Items, Producer_list) ->
    if 
        length(Items) > 0 -> 
            io:format("Consuming item...~n"),
            timer:sleep(7500),
            %Items = lists:delete(Items, 1),
            io:format("Item consumed!~n"),
            producer ! {self(), "producer"},
            consumer(Items, Producer_list);
        length(Items) =< 0 ->
            io:format("No items to consume!~n"),
            timer:sleep(3500),
            consumer(Items, Producer_list)
    end,
    timer:sleep(1000),
    consumer(Items, Producer_list).


% create and print functions


create_producer(Items, MaxItems) ->
    Producer_list = [spawn(fun() -> producer(Items, MaxItems) end) || _ <- lists:seq(1, 4)], %cria 2 produtores
    io:format("Producer created~n"),
    Producer_list.

create_consumer(Items, Producer_list) ->
    Consumer_list = [spawn(fun() -> consumer(Items, Producer_list) end) || _ <- lists:seq(1, 4)], %cria 4 consumidores
    io:format("Consumer created~n"),
    Consumer_list.

print_producer_list(Producer_list) ->
    io:format("Producer list: ~p~n", [Producer_list]).

print_consumer_list(Consumer_list) ->
    io:format("Consumer list: ~p~n", [Consumer_list]).

print_item_list(Items) ->
    io:format("List length: ~p~n", [length(Items)]),
    io:format("Item list: ~p~n", [Items]).