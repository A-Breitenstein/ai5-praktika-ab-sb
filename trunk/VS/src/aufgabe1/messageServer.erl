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
-export([doStartUp/0]).


%% VERTEILTES ERLANG:
%% node wird durch -sname <parameter> gesetzt, host ist der Rechnername || longname wahrscheinlich über verteilte netze
%% <Prozessname> wird durch register(<Prozessname>,<ProzessID>) festgelegt, ProzessID wird durch den Aufruf spawn/1 übergeben
%% {Prozessname, node@host}! {ProzessId,{Message}}.
%% BEISPIEL:
%% {tipOfTheDaymessageServer,messageServer@PIGPEN} ! {prozessname_vom_sender,{getMsgId,node()}}.


%% Einstiegsmethode des MessageServers
doStartUp() ->

  LogFileName = lists:concat([node(),".log"]),
  DefaultConfigFile = "serverconfig.cfg",

  case configFileLoader:loadServerConfig(DefaultConfigFile) of
    {nok,configFileNotOK} ->       werkzeug:logging(LogFileName,"delete config file");
    {nok,fileNotFound} -> configFileLoader:defaultServerSetup(DefaultConfigFile),doStartUp();
    {ok,{ST,CT,MaxDQSize,PN}}->
      MaxIdleTimeServer_TimerRef = erlang:start_timer(ST,self(),shutdownTimeout),
      werkzeug:logging(LogFileName,lists:concat(["+++ server started ", werkzeug:timeMilliSecond()," +++"])),
      queueManagerAsync:start(MaxDQSize),
      %%serverlopp(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,Processname)
      ServerPid = spawn(fun() -> serverLoop([],[],[],ST,MaxIdleTimeServer_TimerRef,CT,MaxDQSize,1,PN) end),
      register(PN,ServerPid),
      ServerPid;
    {nok,ErrMsg} -> werkzeug:logging(LogFileName,ErrMsg)
  end

.



%% versendet die nächste Nachricht an den Client, ist der Client nicht bekannt oder er hat bereits die
%% aktuellste Nachricht, dann bekommt er eine  Dummy Nachricht
sendMessage(Client, DeliveryQueue, ProcessName ) ->
  {{From,RechnerID},{LastSendedMessageID,_TimerRef}} = Client,
  case queueManager:getMessage(DeliveryQueue,LastSendedMessageID) of
    {ok,UpdatedMessageID,Message} ->
        debugOutput(["sending Message to ",From,RechnerID,"   ",Message],""),
        {From,RechnerID} ! {ProcessName,Message}
  end,
  {{From,RechnerID},{UpdatedMessageID,_TimerRef}}
.



%% Die Hauptschleife des Servers die den aktuellen Zustandverwaltet
%% DeliveryQueue(sortedlist)  :: [{ElemNr,{ID,Msg,DeliveryTime}}]
%% HoldbackQueue(sortedlist) :: [{ElemNr,{ID,Msg,ReceiveTime}}]
%% Clients :: [{{From,RechnerID},{lastSendedMessageID,TimerRef}}]
serverLoop(Clients,DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName) ->
  receive

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% get MessageID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {From,{getmsgid,RechnerID}} ->
      debugOutput(lists:concat([" getMessageID called by ",From," on ",RechnerID]),""),
      {From,RechnerID} ! {ProcessName,{nid,MsgID}},
      debugOutput(lists:concat([" send MessageID ",MsgID," to ",RechnerID]),""),
      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
      serverLoop(Clients,DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID+1,ProcessName);


    %%%%%%%%%%%%%%%%%%%%%%%% get message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% mit {tipOfTheDaymessageServer,'messageServer@Sven-PC'} ! {eval_loop,{getMsg,node()}}. aufrufen
    {From,{getmessages,RechnerID}} ->
      debugOutput(lists:concat([" getMessage called by ",From," on ",RechnerID]),""),
      TmpClient = clientManager:findClientByRechnerID(Clients,{From,RechnerID}),
      debugOutput(" current Client ",TmpClient),
      case TmpClient of
          %%%% Wir kennen den Client noch nicht !
          {-1,nok} ->
              {{From,RechnerID},{NewLastSendedMessageID,_}} = sendMessage({{From,RechnerID},{-1, empty}},DeliveryQueue,ProcessName),
              NewClient =  {{From,RechnerID},{NewLastSendedMessageID,erlang:start_timer(ClientTimeout,self(),clientTimeout)}},
              NewClients = clientManager:push(Clients,NewClient);

          %%% Wir kennen den Client !
          {{From,RechnerID},{LastSendedMessageID,TimerRef}} ->
              {{From,RechnerID},{NewLastSendedMessageID,_}} = sendMessage({{From,RechnerID},{LastSendedMessageID,empty}},DeliveryQueue,ProcessName),
              NewTimerRef = refreshTimer(TimerRef,ClientTimeout,clientTimeout),
              NewClient =  {{From,RechnerID},{NewLastSendedMessageID,NewTimerRef}},
              NewClients = clientManager:push(clientManager:removeClient(id,Clients,{From,RechnerID}),NewClient
              )
      end,
      debugOutput(" updated Client",NewClient),
      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
      serverLoop(NewClients,DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName);

    %%%%%%%%%%%%%%%%%%%%%%%%% drop Message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {From,{dropmessage,MsgBlock}} ->
      {MessageID,Msg} = MsgBlock,
      debugOutput(" dropMsg called",MsgBlock),
%%   %%%%%%     synchrone verarbeitung im queueManager %%%%%%%
%%       {NewHBQ,NewDeliveryQueue,_Updated} = queueManager:updateDeliveryQueueIfNecessary(
%%                                         DeliveryQueue,
%%                                         queueManager:enqueueIn(_unUsed_HoldbackQueue,MsgBlock),
%%                                         MaxDQSize),

     %%%%% asynchrone Verarbeitung im queueManager %%%%%%%%%%
      queueManagerAsync:pushHoldbackQueue(MsgBlock),


      NewMaxIdleTimeServer_TimerRef = refreshTimer(MaxIdleTimeServer_TimerRef,MaxIdleTimeServer,shutdownTimeout),
%%       serverLoop(Clients,NewDeliveryQueue,NewHBQ,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName);
      serverLoop(Clients,DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,NewMaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName);


  %%%%%%%%%%%%%%%%%%%%%%% TIMEOUTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {timeout,ClientTimerRef,clientTimeout} ->
      debugOutput(" client timeout",ClientTimerRef),
      serverLoop(clientManager:removeClient(timerRef,Clients,ClientTimerRef),DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName);

    {timeout,ServerTimerRef,shutdownTimeout} ->
      debugOutput(" server timeout",""),
      werkzeug:logstop(),queueManagerAsync:stop();

    {dqUpdated,NewDQ} ->
      debugOutput(list_to_atom(lists:concat(["received updated deliveryqueue from queueManagerAsync index range: ", werkzeug:minNrSL(NewDQ),"  to ", werkzeug:maxNrSL(NewDQ)," , Length ", werkzeug:lengthSL(NewDQ)])),""),
      serverLoop(Clients,NewDQ,_unUsed_HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName);

  %%%%%%%%%%%%%%%%%%%%%%%% RELIEVE ANY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ANY -> debugOutput(" server recived ",ANY),
      serverLoop(Clients,DeliveryQueue,_unUsed_HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID,ProcessName)
  end
.
debugOutput(MSG,ANY) ->
  werkzeug:logging(lists:concat([node(),".log"]), werkzeug:list2String([MSG,ANY]))
.



refreshTimer(TimerRef,TimeInMsec,Msg) ->
  Text = werkzeug:list2String(['refreshing Timer' ,TimerRef,' ',Msg,' ',TimeInMsec,'ms']),
  debugOutput(Text," "),
  erlang:cancel_timer(TimerRef),
  erlang:start_timer(TimeInMsec,self(),Msg)
.