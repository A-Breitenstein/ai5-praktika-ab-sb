%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Okt 2013 22:20
%%%-------------------------------------------------------------------
-module(messageClient).
-author("Sven").

%% API
-export([start/0]).

%% Einstiegsfunktion des MessageClients
start() ->

  DefaultConfigFile = "clientconfig.cfg",
  case configFileLoader:loadClientConfig(DefaultConfigFile) of
    {nok,configFileNotOK} -> "delete config file";
    {nok,fileNotFound} -> configFileLoader:defaultClientSetup(DefaultConfigFile),start();
    {ok,{LT,ServerID,SI,MI,ClientsCount}}->
          spawnClients(ClientsCount,{LT,ServerID,SI,MI,ClientsCount});
    {nok,ErrMsg} -> ErrMsg
  end
.
spawnClients(0,Params) ->
   0
;
spawnClients(ClientCount,Params) ->
  {LT,ServerID,SI,MI,_} = Params,
  Name = list_to_atom(lists:concat(["client_",ClientCount])),
  LogFileName = lists:concat([Name,node(),".log"]),

  ClientPID = spawn(fun() ->
    werkzeug:logging(LogFileName,lists:concat(["+++ client started ", werkzeug:timeMilliSecond()," +++"])),
    erlang:start_timer(LT,self(),lifeTime),
    clientLoop(writer,ServerID,SI,SI,{0,[]},LT,MI,Name,LogFileName)
  end),
  register(Name,ClientPID),
  spawnClients(ClientCount-1,Params)
.


clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename) ->
  ServerID ! {ProcessName,{getmsgid,node()}},
  werkzeug:logging(Logfilename,list_to_atom(lists:concat(["++++++ MessageID angefordert! +++++,  ", werkzeug:timeMilliSecond(),"   currentSendeIntervall in ms: ",round(CurrentSendeIntervall)]))),
  timer:sleep(round(CurrentSendeIntervall)),
  receive
    {From,{nid, Number}} when OutgoingCount < MessagesPerIntervall ->
      Msg = lists:concat([ProcessName,node(),"-VSP/04-Gruppe 1- ID:",Number," Hier kann ihr Text stehn! Client OUT: ", werkzeug:timeMilliSecond()]),
      werkzeug:logging(Logfilename,list_to_atom(Msg)),
      NewSendedMessages = werkzeug:pushSL(SendedMessages,{Number,{Number,Msg}}),
      ServerID ! {ProcessName,{dropmessage,{Number,Msg}}},
      clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount+1,NewSendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename);

    {From,{nid, Number}} when MessagesPerIntervall == OutgoingCount ->
        werkzeug:logging(Logfilename,list_to_atom(lists:concat([Number,"te_Nachricht um ", werkzeug:timeMilliSecond(), " vergessen zu senden ******"]))),
        clientLoop(reader,ServerID,SendeIntervall,getNewSendeIntervall(SendeIntervall,CurrentSendeIntervall),{0,SendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename);

    {timeout,LifeTime_TimerRef,lifeTime} ->
      werkzeug:logging(Logfilename,list_to_atom(lists:concat(["+++++ shutting client down ++++++++ ", werkzeug:timeMilliSecond()])))

  end
;
clientLoop(reader,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename) ->
  ServerID ! {ProcessName,{getmessages,node()}},
  werkzeug:logging(Logfilename,list_to_atom(lists:concat(["******* Message angefordert! *******  ", werkzeug:timeMilliSecond()]))),

  receive
    {From,{reply,ID, MSG,true}}  ->
      clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename)
  ;
    {From,{reply,ID, MSG,false}} ->
      case werkzeug:findSL(SendedMessages,ID) of
      %%% die eingegangene message ist nicht von diesem client gesendet wurden
        {-1,nok} ->
          werkzeug:logging(Logfilename,list_to_atom(MSG));
      %%% dieser client hat die message gesendet
        {ID,{ID,_Message}} ->
          werkzeug:logging(Logfilename,list_to_atom(lists:concat([MSG," +++++++++++ Client IN: ", werkzeug:timeMilliSecond()])))
      end,
      clientLoop(reader,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall,ProcessName,Logfilename);


    {timeout,LifeTime_TimerRef,lifeTime} ->
      werkzeug:logging(Logfilename,list_to_atom(lists:concat(["+++++ shutting client down ++++++++ ", werkzeug:timeMilliSecond()])))

  end

.
%% Berechnet den neuen Sende Intervall der Nachrichten
getNewSendeIntervall(SendeIntervall,CurrentSendeIntervall) ->
  case random:uniform(2) of
    1 -> NewIntervall =   CurrentSendeIntervall + SendeIntervall*0.5 ;
    2 -> NewIntervall =   CurrentSendeIntervall - SendeIntervall*0.5
  end,

  case NewIntervall < 2000 of
    true -> 2000;
    false ->
      case NewIntervall > (2* SendeIntervall) of
        true -> SendeIntervall;
        false -> NewIntervall
      end

  end
.