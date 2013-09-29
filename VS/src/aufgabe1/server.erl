%% Copyright
-module(server).
-author("Alex").

%% API
-export([]).
%%
%% %lol ka vergessen aber nachricht einfügen in den pool
%% addToPool(Message) ->
%%   MessagePool = [MessagePool, Message].
%%
%% %% giveMessages(Id) ->
%% %% ... liste von tupeln aus (reader, MessagePoolIndex)
%% % nach 10 readertupeln wird der erste verdrängt oder so
%% % bei abfrage wird gezählt im MessagePool und ab index rausgegeben
%%
%% loop() ->
%%   receive
%%     {"writer", Message} ->
%%       addToPool(Message)
%%         ;
%%
%%     {"reader", Id} ->
%%       Id ! giveMessages(Id)
%%
%%   end.
