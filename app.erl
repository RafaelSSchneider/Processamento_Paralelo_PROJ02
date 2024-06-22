%precisamos arrumar a forma de deletar o item da lista de itens
%enviar a lista atualizada através de mensagens para os consumidores
% consumidores precisam enviar mensagem aos produtoes disponiveis
% podemos adicionar um receive nos consumidores esperando um produtor
% avisar que um item novo foi criado e esta pronto para ser consumido

-module(app). %define o nome do modulo que será usado
-export([start/0, number_producer/1, consumer/1, buffer/0, buffer/1]). %permite chamar as funções por fora do arquivo

start() ->
    io:format("Programa iniciado!~n"),
    BufferPid = spawn(?MODULE, buffer, []),
    create_producers(BufferPid),
    create_consumers(BufferPid).

number_producer(BufferPid) ->
    receive
        stop ->
            io:format("Producer parando~n")
        after 3500 ->
            RandomNum = rand:uniform(256),
            io:format("Producer de numeros ~p ~n produzindo item ~p~n", [self(), RandomNum]),
            BufferPid ! {produce, RandomNum},
            number_producer(BufferPid)
    end.

consumer(BufferPid) ->
    receive
        stop ->
            io:format("Consumer parando~n")
        after 7500 ->
            io:format("Consumer ~p consumindo~n", [self()]),
            BufferPid ! {consume, self()},
            receive
                {item, none} ->
                    io:format("Buffer vazio, consumer esperando~n"),
                    consumer(BufferPid);
                {item, Item} ->
                    io:format("Consumer ~p consumiu item ~p~n", [self(), Item]),
                    consumer(BufferPid)
            end
    end.

buffer() ->
    buffer([]).

buffer(Items) ->
    receive
        {produce, Item} ->
            NewItems = Items ++ [Item],
            io:format("Buffer recebeu item ~p, novo buffer: ~w~n", [Item, NewItems]),
            buffer(NewItems);
        
        {consume, ConsumerPid} ->
            io:format("Consumer ~p solicitou para consumir um item~n", [ConsumerPid]),
            case Items of
                [] ->
                    io:format("Buffer vazio~n"),
                    ConsumerPid ! {item, none},
                    buffer(Items);
                [Head | Tail] ->
                    io:format("Item ~p sendo consumido pelo consumer ~p~n", [Head, ConsumerPid]),
                    ConsumerPid ! {item, Head},
                    buffer(Tail)
            end
    end.

% Cria 2 produtores
create_producers(BufferPid) ->
    spawn(?MODULE, number_producer, [BufferPid]),
    spawn(?MODULE, number_producer, [BufferPid]).

% Cria 4 consumidores
create_consumers(BufferPid) ->
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]),
    spawn(?MODULE, consumer, [BufferPid]).

%% print_producer_list(Producer_list) -> io:format("Producer list: ~p~n", [Producer_list]).

%% print_consumer_list(Consumer_list) -> io:format("Consumer list: ~p~n", [Consumer_list]).

%% print_item_list(Items) -> io:format("List length: ~p~n", [length(Items)]), io:format("Item list: ~p~n", [Items]).
