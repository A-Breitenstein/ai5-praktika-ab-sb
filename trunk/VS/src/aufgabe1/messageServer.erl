%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Okt 2013 22:28
%%%-------------------------------------------------------------------
-module(messageServer).
-author("Sven").

%% API
-export([doStartUp/1,doStartUp/0,sammelBisZurLuecke/1]).


%% VERTEILTES ERLANG(for shorties mit nodes):
%% node wird durch -sname <parameter> gesetzt, host ist der Rechnername || longname warscheinlich über verteilte netze
%% <Prozessname> wird durch register(<Prozessname>,<ProzessID>) festgelegt, ProzessID wird durch den Aufruf spawn/1 übergeben
%% {Prozessname, node@host}! {ProzessId,{Message}}.
%% BEISPIEL:
%% {tipOfTheDaymessageServer,messageServer@PIGPEN} ! {prozessname_vom_sender,{getMsgId,node()}}.

doStartUp() ->
  ServerPid = spawn(fun() -> doStartUp([]) end),
    register(tipOfTheDaymessageServer,ServerPid),
  ServerPid
.
doStartUp(Args) ->
  DefaultConfigFile = "serverconfig.cfg",
  LogFileName = lists:concat([node(),".log"]),
  ConfigFile = file:consult(DefaultConfigFile),
  case ConfigFile of
  {ok,ConfigList} ->
      ServerTimeout = werkzeug:get_config_value(serverTimeout,ConfigList),
      ClientTimeout = werkzeug:get_config_value(clientTimeout,ConfigList),
      MaxMessageCount = werkzeug:get_config_value(maxMessageCount,ConfigList),

      {ok,ST} = ServerTimeout,
      {ok,CT} = ClientTimeout,
      {ok,MMC} = MaxMessageCount,
      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok end,[ServerTimeout,ClientTimeout,MaxMessageCount]) of
        true ->
          MaxIdleTimeServer_TimerRef = erlang:start_timer(ST,self(),shutdownTimeout),
          werkzeug:logging(LogFileName,lists:concat(["+++ server started ",werkzeug:timeMilliSecond()," +++"])),

          %%(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID)
          %% [{12,{12,"Text qwe 121212,",12331}},{11,{11,"Text qwe 11111,",1111}},{10,{10,"Text qwe 101019,",12331}}]
          serverLoop([],[],[],ST,MaxIdleTimeServer_TimerRef,CT,MMC,1);

        false ->
          werkzeug:logging(LogFileName,"Configfile values arent ok!")


      end;

    %% enoent seams to be the file not found / exsists error, see file:consult(...) docu
    {error,enoent} ->
      defaultServerSetup(DefaultConfigFile),
      doStartUp(Args);
    {error,ErrMsg}->
      werkzeug:logging(LogFileName,ErrMsg)
  end, 0
.

defaultServerSetup(FileName) ->
  InitConfig = "{serverTimeout, 400000}.\n{clientTimeout, 12000}.\n{maxMessageCount, 125}.\n",
  file:write_file(FileName, InitConfig, [append])
.

sendMessage(From,Client, DeliveryQueue ) ->
  %%%% Wir kennen den Client
  {RechnerID,{LastSendedMessageID,TimerRef}} = Client,
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

      {From,RechnerID} ! {tipOfTheDaymessageServer,{reply,ID, lists:concat([Msg,"  SERVER OUT: ",werkzeug:timeMilliSecond()]),not((HighestMessageID-ID) > 0)}},
      debugOutput(lists:concat(["sending Message to ",RechnerID,"  ",reply,"  ",ID, "  ", lists:concat([Msg,"  SERVER OUT: ",werkzeug:timeMilliSecond()])," ",not((HighestMessageID-ID) > 0)])," "),
      {RechnerID,{ID,TimerRef}};

    %%% es keine neuen  Messages
    false ->
      {From,RechnerID} ! {tipOfTheDaymessageServer,{reply,-1, "dummy message ...",true}},
      debugOutput(lists:concat(["sending Dummy Message to ",RechnerID,"  ",reply," ",-1, " dummy message ..."]),""),
      {RechnerID,{LastSendedMessageID,TimerRef}}
  end
.




%% DeliveryQueue(sortedlist)  :: [{ElemNr,{ID,Msg,DeliveryTime}}]
%% HoldbackQueue(sortedlist) :: [{ElemNr,{ID,Msg,ReceiveTime}}]
%% Clients :: [{RechnerID,{lastSendedMessageID,TimerRef}}]

serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID) ->
  receive

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% get MessageID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {From,{getmsgid,RechnerID}} ->
      debugOutput(lists:concat([" getMessageID called by ",From," on ",RechnerID]),""),
      {From,RechnerID} ! {tipOfTheDaymessageServer,{nid,MsgID}},
      debugOutput(lists:concat([" send MessageID ",MsgID," to ",RechnerID]),""),
      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID+1);


    %%%%%%%%%%%%%%%%%%%%%%%% get message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% mit {tipOfTheDaymessageServer,'messageServer@Sven-PC'} ! {eval_loop,{getMsg,node()}}. aufrufen
    %%% bei aktuellem Setup funktioniert es
    {From,{getmessages,RechnerID}} ->
      debugOutput(lists:concat([" getMessage called by ",From," on ",RechnerID]),""),

      debugOutput(" current Client ",werkzeug:findSL(Clients,RechnerID)),
      case werkzeug:findSL(Clients,RechnerID) of
          %%%% Wir kennen den Client noch nicht !
          {-1,nok} ->
              Client = sendMessage(From,{RechnerID,{-1, erlang:start_timer(ClientTimeout,self(),clientTimeout)}},DeliveryQueue),
              NewClients = werkzeug:pushSL(Clients,Client);
          %%% Wir kennen den Client !
          {RechnerID,{LastSendedMessageID,TimerRef}} ->
              NewTimerRef = refreshTimer(TimerRef,ClientTimeout,clientTimeout),
              Client = sendMessage(From,{RechnerID,{LastSendedMessageID,NewTimerRef}},DeliveryQueue),
              NewClients = werkzeug:pushSL(lists:filter(fun(Item)-> {ItemRechnerID,{_MsgID,_TimerRef}} = Item,  RechnerID =/= ItemRechnerID end,Clients),Client)
      end,
      debugOutput(" updated Client",Client),
      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
      serverLoop(NewClients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

    %%%%%%%%%%%%%%%%%%%%%%%%% drop Message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     {From,{qwe123,SenderID,AbsendeZeit,{Msg,MessageID}}} ->
    {From,{dropmessage,MsgBlock}} ->
      {MessageID,Msg} = MsgBlock,
      debugOutput(" dropMsg called",MsgBlock),

      %% Incoming Message already in Holdbackqueue?
      case lists:any(fun(Item) -> {ElemNr,Elem} = Item,MessageID =:= ElemNr end,HoldbackQueue)of
        true->
          NewHBQ = HoldbackQueue,
          debugOutput('Message already in Holdbackqueue : ',MessageID)  ;
        false->
          NewHBQ = werkzeug:pushSL(HoldbackQueue,{MessageID,{MessageID,lists:concat([Msg," IN HBQ: ",werkzeug:timeMilliSecond()]),time()}}),
          debugOutput('Message has been pushed into Holdbackqueue : ',MessageID)
      end,

      case werkzeug:lengthSL(NewHBQ) > (MaxDQSize * 0.5) of
        true ->
          Test_HighestMessageID_DQ = werkzeug:maxNrSL(DeliveryQueue) ,
          case Test_HighestMessageID_DQ < 0  of
            true -> SuccessorOf_HighestMessageID_DQ = 1;
            false -> SuccessorOf_HighestMessageID_DQ = Test_HighestMessageID_DQ + 1
          end,
          debugOutput(werkzeug:list2String(['Successor of Highest MessageID of DQ',SuccessorOf_HighestMessageID_DQ]),""),
%%           debugOutput(">>>>>>>>>>>>>>>>>>>>Liste NewHBQ: ", NewHBQ),
          LowestHBQ_Elem = werkzeug:findSL(NewHBQ,werkzeug:minNrSL(NewHBQ)),
%%           debugOutput(">>>>>>>>>>>>>>>>>>>>LowestHBQ_Elem: ", LowestHBQ_Elem),
          %%{ElemNr,{ID,Msg,ReceiveTime}}]
          {ElemNr,_Elem} = LowestHBQ_Elem,
          PredessorOfElemNr = ElemNr -1,
          debugOutput(werkzeug:list2String(['Predessor of Lowest Elem of HBQ ',PredessorOfElemNr]),""),
          case (ElemNr - SuccessorOf_HighestMessageID_DQ ) > 0 of
            true->
              debugOutput(werkzeug:list2String(['Gap found : Fehlernachricht fuer Nachrichtennummern ',SuccessorOf_HighestMessageID_DQ,' bis ',PredessorOfElemNr,' um '])," "),
              NewDQ = werkzeug:pushSL(DeliveryQueue,{PredessorOfElemNr,{PredessorOfElemNr,werkzeug:list2String(['Fehlernachricht fuer Nachrichtennummern ',SuccessorOf_HighestMessageID_DQ,' bis ',PredessorOfElemNr,' um ']),werkzeug:timeMilliSecond()}}),
              {GatheredMessages,_Index,NewNewHBQ} = sammelBisZurLuecke(NewHBQ),
              debugOutput(werkzeug:list2String(['GatheredMessags : ',GatheredMessages,' NewNewHBQ :',NewNewHBQ,' um '])," "),
              NewDeliveryQueue = lists:sublist(lists:append(GatheredMessages,NewDQ),MaxDQSize)
          ;
            false -> {GatheredMessages,_Index,NewNewHBQ} = sammelBisZurLuecke(NewHBQ),
              debugOutput(werkzeug:list2String(['no Gap found :GatheredMessags : ',GatheredMessages,' NewNewHBQ :',NewNewHBQ,' um '])," "),
              NewDeliveryQueue = lists:sublist(lists:append(GatheredMessages,DeliveryQueue),MaxDQSize)
          end,


          debugOutput('NewDeliveryQueue ',NewDeliveryQueue)
      ;

        false ->
          debugOutput(werkzeug:list2String(['NewHBQ filled by ',werkzeug:lengthSL(NewHBQ) / (MaxDQSize)*100, ' % ']),""),
          NewNewHBQ = NewHBQ, NewDeliveryQueue = DeliveryQueue
      end,


      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
      serverLoop(Clients,NewDeliveryQueue,NewNewHBQ,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);


  %%%%%%%%%%%%%%%%%%%%%%% TIMEOUTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {timeout,ClientTimerRef,clientTimeout} ->
      debugOutput(" client timeout",ClientTimerRef),
      NewClients = lists:filter(fun(Item)-> {_RechnerID,{_MsgID,TimerRef}} = Item, TimerRef =/= ClientTimerRef end,Clients),
      serverLoop(NewClients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

    {timeout,ServerTimerRef,shutdownTimeout} ->
      debugOutput(" server timeout",""),
      werkzeug:logstop();

  %%%%%%%%%%%%%%%%%%%%%%%% RELIEVE ANY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ANY -> debugOutput(" server recived ",ANY),
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID)
  end
.
debugOutput(MSG,ANY) ->
  werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String([MSG,ANY]))
.


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
refreshTimer(TimerRef,TimeInMsec,Msg) ->
  Text = werkzeug:list2String(['refreshing Timer' ,TimerRef,' ',Msg,' ',TimeInMsec,'ms']),
  debugOutput(Text," "),
  erlang:cancel_timer(TimerRef),
  erlang:start_timer(TimeInMsec,self(),Msg)
.