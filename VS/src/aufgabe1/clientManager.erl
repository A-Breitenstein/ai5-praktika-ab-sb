%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Okt 2013 19:20
%%%-------------------------------------------------------------------
-module(clientManager).
-author("Sven").

%% API
-export([removeClient/3, findClientByRechnerID/2, push/2]).
%% entfernt einen Client anhand seiner TimerRef bzw der ID aus der Clientslist
removeClient(timerRef,Clients,ClientTimerRef) ->
  lists:filter(
      fun(Item)-> {{_From,_RechnerID},{_MsgID,TimerRef}} = Item,
                   TimerRef =/= ClientTimerRef
      end
    ,Clients)
;
removeClient(id,Clients,ID) ->
  lists:filter(
    fun(Item)->
      {ItemRechnerID,{_MsgID,_TimerRef}} = Item,
        ID =/= ItemRechnerID
    end
    ,Clients)
.
%% findet einen Client in der Clients liste anhand seiner ID {ID,Client} bei erfolg, bei nicht auffinden {-1,nok}
findClientByRechnerID(Clients,RechnerID) ->
  werkzeug:findSL(Clients,RechnerID)
.
%% fügt einen Client in die Clients liste ein und liefert die neue liste zurück
push(Clients,Client) -> werkzeug:pushSL(Clients,Client).