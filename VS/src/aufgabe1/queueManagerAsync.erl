%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Okt 2013 12:58
%%%-------------------------------------------------------------------
-module(queueManagerAsync).
-author("Sven").

%% API
-export([start/1, stop/0, pushHoldbackQueue/1]).

%%
%%
%%
%% Dieses Modul erweitert den queueManager, es lagert den QueueManager in einen eigenen Thread aus
%%
%%

start(MaxDQSize) ->
  Known = erlang:whereis(queueManagerAsync),
  case Known of
    undefined -> PIDlogklc = spawn(fun() -> loop([],[],MaxDQSize) end),
      erlang:register(queueManagerAsync,PIDlogklc);
    _NotUndef -> ok
  end,

  ok.

stop() -> 	Known = erlang:whereis(queueManagerAsync),
  case Known of
    undefined -> false;
    _NotUndef -> queueManagerAsync ! kill, true
  end.

loop(Holdbackqueue,Deliveryqueue,MaxDQSize) ->
  receive
    {From,pushHBQ, MsgBlock} ->
      {NewHBQ,NewDeliveryQueue,DQUpdated} = queueManager:updateDeliveryQueueIfNecessary(
                  Deliveryqueue,
                  queueManager:enqueueIn(Holdbackqueue,MsgBlock),
                  MaxDQSize),
      case DQUpdated of
        true -> From ! {dqUpdated,NewDeliveryQueue};
        false -> 0
      end,
      loop(NewHBQ,NewDeliveryQueue,MaxDQSize);


    kill -> true
  end.
pushHoldbackQueue(MsgBlock) ->
  queueManagerAsync ! {self(),pushHBQ,MsgBlock}
.