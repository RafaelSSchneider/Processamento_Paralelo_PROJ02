# Processamento_Paralelo_PROJ02


Repositório para a matéria de processamento paralelo ministrador por PAULO RICARDO MUNIZ BARROS.
Trabalho com utilização de Erlang para criar um sistema que simule a implementação de um sistema com dois (02) produtores, que produzem itens aleatoriamente diferentes (dois tipos) e outros quatro (04) consumidores, com tempo de criação de 3,5 segundos e tempo para consumir o produto de 7,5 segundo, sendo o dobro do criado.

![Screenshot_1](https://github.com/RafaelSSchneider/Processamento_Paralelo_PROJ02/assets/20550252/5a16a19f-65d3-40fe-b093-ef93ad8fd8e5)


Podemos usar uma recursividade para iniciar a lista, caso os producer consigam lotar a fila, deixamos eles aguardando uma mensagem com o sistema de mensagem do erlang falando que um produto foi consumido, essa mensagem será enviado do consumer, 

(podendo mudar o diagrama)
