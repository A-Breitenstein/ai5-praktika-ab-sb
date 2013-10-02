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
-export([doStartUp/1,doStartUp/0]).


%% VERTEILTES ERLANG(for shorties mit nodes):
%% node wird durch -sname <parameter> gesetzt, host ist der Rechnername || longname warscheinlich über verteilte netze
%% <Prozessname> wird durch register(<Prozessname>,<ProzessID>) festgelegt, ProzessID wird durch den Aufruf spawn/1 übergeben
%% {Prozessname, node@host}! {ProzessId,{Message}}.
%% BEISPIEL:
%% {tipOfTheDaymessageServer,messageServer@PIGPEN} ! {prozessname_vom_sender,{getMsgId,node()}}.

doStartUp() ->
  ServerPid = spawn(fun() -> doStartUp([]) end),
    register(tipOfTheDaymessageServer,ServerPid),
  debugOutput("ServerPID",ServerPid),
  pman:start(),
  net_kernel:start([messageServer, shortnames]),
  debugOutput("node()",node()),
  ServerPid
.
doStartUp(Args) ->
  DefaultConfigFile = "serverconfig.cfg",
  ConfigFile = file:consult(DefaultConfigFile),
  case ConfigFile of
    {ok,ConfigList} ->
      ServerTimeout = get_value_of(serverTimeout,ConfigList),
      ClientTimeout = get_value_of(clientTimeout,ConfigList),
      MaxMessageCount = get_value_of(maxMessageCount,ConfigList),

      debugOutput("ConfigList",ConfigList),
      debugOutput("ServerTimeout",ServerTimeout),
      debugOutput("ClientTimeout",ClientTimeout),
      debugOutput("MaxMessageCount",MaxMessageCount),

      {ok,ST} = ServerTimeout,
      {ok,CT} = ClientTimeout,
      {ok,MMC} = MaxMessageCount,
      case true of
        true ->
          MaxIdleTimeServer_TimerRef = erlang:start_timer(ST,self(),shutdownTimeout),

          %%(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID)
          serverLoop(dict:new(),queue:new(),queue:new(),ST,MaxIdleTimeServer_TimerRef,CT,MMC,0);
        false -> io:format(" reason :~p\n", ["Configfile values arent ok!"])

      end;

    %% enoent seams to be the file not found / exsists error, see file:consult(...) docu
    {error,enoent} ->
      defaultServerSetup(DefaultConfigFile),
      doStartUp(Args);
    {error,ErrMsg}->
      io:format("Loading config file failed, reason :~p\n", [ErrMsg])
  end, 0
.

defaultServerSetup(FileName) ->
  InitConfig = "{serverTimeout, 400000}.\n{clientTimeout, 12000}.\n{maxMessageCount, 125}.\n",
  file:write_file(FileName, InitConfig, [append])
.
%%%%  aus der BA_Erlang von Klauck entnommen
get_value_of(_Key, []) ->
  {error, not_found};
get_value_of(Key, [{Key, Value} | _Tail]) ->
  {ok, Value};
get_value_of(Key, [{_Other, _Value} | Tail]) ->
  get_value_of(Key, Tail).

checkValueAvailable(List) ->
 0
;
checkValueAvailable([]) ->
 0
.

%% Clients Map [{rechnerId,{lastRequestedMessage,timeoutTimer}}]
%% DeliveryQueue [{}]



max(Maxima,[]) -> Maxima;
max(Maxima, [Item,Rest])->

  {_SenderID_, _Zeit_, _Nachricht_, MessageID} = Item,

  case Maxima < MessageID of
    true -> max(MessageID,Rest);
    flase -> max(Maxima,Rest)
  end
.

getItem(NewHBQ, HighestDQID) ->
  Find = fun(Item) ->
    {_SenderID_, _Zeit_, _Nachricht_, MessageID} = Item,
     HighestDQID =:= MessageID
  end,

  queue:filter(Find,NewHBQ)
.

addMessagesToDQ(NewHBQ, DeliveryQueue,MaxDQSize) ->
  HighestDQID = max(0,queue:to_list(DeliveryQueue)),


  case queue:is_empty(getItem(NewHBQ,HighestDQID+1)) of
    true -> 0;%ErstelleFehlerMessage->packInBagTillGap
    false -> 0%packInBagTillGap
  end


.


que_adjustment(NewHBQ, DeliveryQueue, MaxDQSize) ->


  case queue:len(NewHBQ)>(MaxDQSize*0.5) of
    true-> addMessagesToDQ(NewHBQ, DeliveryQueue, MaxDQSize);
    false->0

  end
.

%% serverLoop(Clients,DeliveryQueue,HoldbackQueue,Msg) ->
serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID) ->
  receive
    {From,{getMsgId,RechnerID}} ->
      debugOutput("getMsgId called",From),
      NewMsgID = MsgID + 1,
      {From,RechnerID} ! {getMsgId,NewMsgID},
      debugOutput("send getMsgId ",[{From,RechnerID},{getMsgId,NewMsgID}]),
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,NewMsgID);


    {From,{dropMsg,SenderID, Zeit, Nachricht,MessageID}}->
      debugOutput("dropMsg called",From),

      NewHBQ = addMesseageToHBQ(HoldbackQueue, SenderID, Zeit, Nachricht,MessageID),
      debugOutput("NewHBQ",NewHBQ),
%%       ACK der Message
      {From,RechnerID} ! {dropMsg,MessageID},
      que_adjustment(NewHBQ, DeliveryQueue, MaxDQSize),
      serverLoop(Clients,DeliveryQueue,NewHBQ,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

    {From,{getMsg,RechnerID}} ->
      debugOutput("getMsg called",From),
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

    {timeout,ClientTimerRef,clientTimeout} -> 0;
    {timeout,ServerTimerRef,shutdownTimeout} -> debugOutput("server timeout",[]);

    ANY -> debugOutput("server recived ",ANY)
  end
.
debugOutput(MSG,ANY) ->
  io:format("debug output: ~p :: ~p\n", [MSG,ANY])
.

addMesseageToHBQ(HoldbackQueue, SenderID, Zeit, Nachricht, MessageID) ->

  ContainsID = fun(Item) -> {SenderID_, Zeit_, Nachricht_, MessageID_} = Item,
    MessageID =:= MessageID_
  end,

  case  queue:is_empty(queue:filter(ContainsID,HoldbackQueue)) of
    false ->  NEW = HoldbackQueue;
    true -> NEW = queue:in({SenderID, Zeit, Nachricht, MessageID},HoldbackQueue)
  end,
  NEW
.


refreshTimer(TimerRef,TimeInMsec,Msg) ->
  erlang:cancel_timer(TimerRef),
  erlang:start_timer(TimeInMsec,self(),Msg)
.