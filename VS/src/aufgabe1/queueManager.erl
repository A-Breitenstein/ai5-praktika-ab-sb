%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Okt 2013 10:14
%%%-------------------------------------------------------------------
-module(queueManager).
-author("Sven").

%% API
-export([getMessage/2,enqueueIn/2,updateDeliveryQueueIfNecessary/3]).

%% liefert die nächste Message für einen gegeben MessageID aus der DeliveryQueue zurück,
%% gibt es keine nächste Message wird eine Dummy Message zurück gegeben
%% (DeliveryQueue(SortedList) x MessageID) -> {ok,NewMessageID,Message}
getMessage(DeliveryQueue,LastSendedMessageID) ->
  HighestMessageID = werkzeug:maxNrSL(DeliveryQueue),
  LowestMessageID = werkzeug:minNrSL(DeliveryQueue),

  case LastSendedMessageID < HighestMessageID andalso HighestMessageID >= 0 of
  %%% es gibt mindestens eine neue Message
    true ->
      case LastSendedMessageID >= LowestMessageID of
        true ->
          {Nr,{ID,Msg,_DeliveryTime}} = werkzeug:findneSL(DeliveryQueue,LastSendedMessageID + 1);
        false ->
          {Nr,{ID,Msg,_DeliveryTime}} = werkzeug:findneSL(DeliveryQueue,LowestMessageID)
      end,

     {ok,ID,{reply,ID, lists:concat([Msg,"  SERVER OUT: ",werkzeug:timeMilliSecond()]),not((HighestMessageID-ID) > 0)}};

  %%% es keine neuen  Messages
    false ->
      {ok,LastSendedMessageID,{reply,-1, "dummy message ...",true}}

end
.
%% Hängt in eine Sortierte Liste (Profs. Klaucks impl :: [{2,_Elem},{1,_Elem}]) ein Element ein,
%% wenn es das Element mit der Nummer X in der Sortierten Liste noch nicht gibt.
%% (Queue(SortedList) x Elem ) -> UpdatedQueue(SortedList)
enqueueIn(HoldbackQueue,MsgBlock) ->
  {MessageID,Msg} = MsgBlock,
  %% Incoming Message already in Holdbackqueue?
  case lists:any(fun(Item) -> {ElemNr,Elem} = Item,MessageID =:= ElemNr end,HoldbackQueue)of
    true->
      werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['Message already in Holdbackqueue : ',MessageID])),
      HoldbackQueue;
    false->
      werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['Message has been pushed into Holdbackqueue : ',MessageID])),
      werkzeug:pushSL(HoldbackQueue,{MessageID,{MessageID,lists:concat([Msg," IN HBQ: ",werkzeug:timeMilliSecond()]),time()}})
  end
.

%% Prüft ob die Holdbackqueue zur hälfte gefüllt ist und füllt Notfalls die Messages aus der Holdbackqueue
%% in die Deliveryqueue ( ohne lücken)
%% (DeliveryQueue(SortedList) x NewHBQ(SortedList) x Integer) ->  {NewNewHBQ(SortedList),NewDeliveryQueue(SortedList)}
updateDeliveryQueueIfNecessary(DeliveryQueue,NewHBQ,MaxDQSize) ->
  case werkzeug:lengthSL(NewHBQ) > (MaxDQSize * 0.5) of
    true ->
      Test_HighestMessageID_DQ = werkzeug:maxNrSL(DeliveryQueue) ,
      case Test_HighestMessageID_DQ < 0  of
        true -> SuccessorOf_HighestMessageID_DQ = 1;
        false -> SuccessorOf_HighestMessageID_DQ = Test_HighestMessageID_DQ + 1
      end,
      werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['Successor of Highest MessageID of DQ',SuccessorOf_HighestMessageID_DQ])),

%%           debugOutput(">>>>>>>>>>>>>>>>>>>>Liste NewHBQ: ", NewHBQ),
      LowestHBQ_Elem = werkzeug:findSL(NewHBQ,werkzeug:minNrSL(NewHBQ)),
%%           debugOutput(">>>>>>>>>>>>>>>>>>>>LowestHBQ_Elem: ", LowestHBQ_Elem),
      %%{ElemNr,{ID,Msg,ReceiveTime}}]
      {ElemNr,_Elem} = LowestHBQ_Elem,
      PredessorOfElemNr = ElemNr -1,
      werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['Predessor of Lowest Elem of HBQ ',PredessorOfElemNr])),
      case (ElemNr - SuccessorOf_HighestMessageID_DQ ) > 0 of
        true->
          werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['Gap found : Fehlernachricht fuer Nachrichtennummern ',SuccessorOf_HighestMessageID_DQ,' bis ',PredessorOfElemNr,' um '])),
          NewDQ = werkzeug:pushSL(DeliveryQueue,{PredessorOfElemNr,{PredessorOfElemNr,werkzeug:list2String(['Fehlernachricht fuer Nachrichtennummern ',SuccessorOf_HighestMessageID_DQ,' bis ',PredessorOfElemNr,' um ']),werkzeug:timeMilliSecond()}}),
          {GatheredMessages,_Index,NewNewHBQ} = sammelBisZurLuecke(NewHBQ),
          werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String(['GatheredMessags : ',GatheredMessages,' NewNewHBQ :',NewNewHBQ,' um '])),
          NewDeliveryQueue = lists:sublist(lists:append(GatheredMessages,NewDQ),MaxDQSize)
      ;
        false ->
          {GatheredMessages,_Index,NewNewHBQ} = sammelBisZurLuecke(NewHBQ),
          werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String(['no Gap found :GatheredMessags : ',GatheredMessages,' NewNewHBQ :',NewNewHBQ,' um '])),
          NewDeliveryQueue = lists:sublist(lists:append(GatheredMessages,DeliveryQueue),MaxDQSize)
      end,

      Updated = true,
      werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String(['NewDeliveryQueue ',NewDeliveryQueue]));

    false ->
      werkzeug:logging(lists:concat([node(),".log"]),lists:concat(['NewHBQ filled by ',round(werkzeug:lengthSL(NewHBQ) / (MaxDQSize)*100), ' % '])),
      NewNewHBQ = NewHBQ,
      NewDeliveryQueue = DeliveryQueue,
      Updated = false

  end,
  {NewNewHBQ,NewDeliveryQueue,Updated}
.

%% Sammelt in der HoldbackQueue vom kleinsten Element an alle Messages ein bis eine Lücke auftritt (  in GatheredItems ),
%% danach wird der Rest der Holdbackqueue eingesammelt ( in NewHBQ ) und so die neue Holdbackqueue erstellt.
%% (HoldbackQueue(SortedList)) -> {GatheredItems,Index,NewHBQ}
%%sammelBisZurLuecke([{7,{1,qwe,qwe}},{5,{qwe,qwe,qwe}},{2,{qwe,qwe,qwe}},{1,{qwe,qwe,qwe}}]) => {[{2,{qwe,qwe,qwe}},{1,{qwe,qwe,qwe}}],7, [{7,{1,qwe,qwe}},{5,{qwe,qwe,qwe}}]}
sammelBisZurLuecke(HoldbackQueue) ->

  lists:foldr(fun(Item,Accu) ->
    {ElemNr,{ID,Msg,ReceiveTime}} = Item,
    {GatheredItems,Index,NewHBQ} = Accu,
%%      debugOutput(werkzeug:list2String(["index: ",Index," ElemNr ",ElemNr]),""),
    case Index =:= ElemNr of
      true->
        NewItem = {ElemNr,{ID,lists:concat([Msg,"   IN DQ:  ",werkzeug:timeMilliSecond()]),['DeliveryTime: ',time(), ' ReciveTime: ',ReceiveTime]}},
        NewGatheredItems =  werkzeug:pushSL(GatheredItems,NewItem),
        AccuOut = {NewGatheredItems,ElemNr+1,NewHBQ};
      false->
        NewNewHBQ = werkzeug:pushSL(NewHBQ,Item),
        AccuOut = {GatheredItems,Index,NewNewHBQ}
    end

  end,{[],werkzeug:minNrSL(HoldbackQueue),[]},HoldbackQueue)
.