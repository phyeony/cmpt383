-module(priority_queue).
-export([new/0, insert/2, delete_min/1]).

new() -> [].

insert(Item, Queue) ->
    insert(Item, Queue, []).

insert(Item, [], Acc) ->
    lists:reverse([Item | Acc]);
insert(Item, [Head | Tail], Acc) when Item#item.priority < Head#item.priority ->
    lists:reverse(Acc) ++ [Item | [Head | Tail]];
insert(Item, [Head | Tail], Acc) ->
    insert(Item, Tail, [Head | Acc]).

delete_min(Queue) ->
    delete_min(Queue, [], _Min, Rest),
    Rest.

delete_min([], Acc, Min, Acc) ->
    Min;
delete_min([Item | Rest], Acc, Min, RestAcc) when Item#item.priority < Min#item.priority ->
    delete_min(Rest, [Item | Acc], Item, RestAcc);
delete_min([Item | Rest], Acc, Min, RestAcc) ->
    delete_min(Rest, [Min | Acc], Min, [Item | RestAcc]).
