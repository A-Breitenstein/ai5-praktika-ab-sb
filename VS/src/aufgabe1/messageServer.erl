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
          serverLoop([],[],[],ST,MaxIdleTimeServer_TimerRef,CT,MMC,0);
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


%% DeliveryQueue(sortedlist)  :: [{ElemNr,{ID,Msg,DeliveryTime}}]
%% HoldbackQueue(sortedlist) :: [{ElemNr,{ID,Msg,ReceiveTime}}]
%% Clients :: [{RechnerID,{lastSendedMessageID,TimerRef}}]
serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID) ->
  receive
    {From,{getMsgId,RechnerID}} ->
      debugOutput("getMsgId called",From),
      NewMsgID = MsgID + 1,
      debugOutput("send getMsgId ",[{From,RechnerID},{getMsgId,NewMsgID}]),
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,NewMsgID);

    {From,{getMsg,RechnerID}} ->
      debugOutput("getMsg called",From),

      LastSendedMessageID = getLastMessageOfClient(Clients,RechnerID),

      %% kann -1 liefern wenn liste leer ist
      HighestMessageID = werkzeug:maxNrSL(DeliveryQueue),
      case LastSendedMessageID < HighestMessageID of
        true ->
          {Nr,{_Nr,Msg,_DeliveryTime}} = werkzeug:findSL(DeliveryQueue,LastSendedMessageID + 1),
          {From,RechnerID} ! {getMsg,LastSendedMessageID + 1, Msg};
        false -> {From,RechnerID} ! {getMsg,-1, "dummy message ..."}
      end,
      serverLoop(Clients,DeliveryQueue,HoldbackQueue,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

{From,{dropMsg,SenderID, Zeit, Nachricht,MessageID}}->
debugOutput("dropMsg called",From),

NewHBQ = addMesseageToHBQ(HoldbackQueue, SenderID, Zeit, Nachricht,MessageID),
debugOutput("NewHBQ",NewHBQ),
%%       ACK der Message
{From,RechnerID} ! {dropMsg,MessageID},
que_adjustment(NewHBQ, DeliveryQueue, MaxDQSize),
serverLoop(Clients,DeliveryQueue,NewHBQ,MaxIdleTimeServer,MaxIdleTimeServer_TimerRef,ClientTimeout,MaxDQSize,MsgID);

{timeout,ClientTimerRef,clientTimeout} -> 0;
{timeout,ServerTimerRef,shutdownTimeout} -> debugOutput("server timeout",[]);

ANY -> debugOutput("server recived ",ANY)
end
.
debugOutput(MSG,ANY) ->
  io:format("debug output: ~p :: ~p\n", [MSG,ANY])
.




refreshTimer(TimerRef,TimeInMsec,Msg) ->
  erlang:cancel_timer(TimerRef),
  erlang:start_timer(TimeInMsec,self(),Msg)
.