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

start() ->
  ClientPID = spawn(fun() -> doStartUp() end),
  register(messageClient,ClientPID),
  ClientPID
.



doStartUp() ->
  DefaultConfigFile = "clientconfig.cfg",
  LogFileName = lists:concat([node(),".log"]),
  ConfigFile = file:consult(DefaultConfigFile),
  case ConfigFile of
    {ok,ConfigList} ->
      Clients = werkzeug:get_config_value(clients,ConfigList),
      LifeTime = werkzeug:get_config_value(lifetime,ConfigList),
      Server = werkzeug:get_config_value(serverid,ConfigList),
      SendeIntervall = werkzeug:get_config_value(sendeintervall,ConfigList),
      MessagesIntervall = werkzeug:get_config_value(messagesperintervall,ConfigList),


      {ok,LT} = LifeTime,
      {ok,ServerID} = Server,
      {ok,SI} = SendeIntervall,
      {ok,MI} = MessagesIntervall,
      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok end,[LifeTime,Server,SendeIntervall,MessagesIntervall]) of
        true ->
          LifeTime_TimerRef = erlang:start_timer(LT,self(),lifeTime),
          werkzeug:logging(LogFileName,lists:concat(["+++ client started ",werkzeug:timeMilliSecond()," +++"])),

          clientLoop(writer,ServerID,SI,SI,{0,[]},LT,MI);

        false ->
           werkzeug:logging(LogFileName,"Configfile values arent ok!")


      end;

  %% enoent seams to be the file not found / exsists error, see file:consult(...) docu
  {error,enoent} ->
      defaultClientSetup(DefaultConfigFile),
      doStartUp();
  {error,ErrMsg}->
      werkzeug:logging(LogFileName,ErrMsg)
  end
.

defaultClientSetup(FileName) ->

  InitConfig =  "{clients, 9}.\n{lifetime, 90000}.\n{serverid, {tipOfTheDaymessageServer,'messageServer@Sven-PC'}}.\n{sendeintervall, 5000}.\n{messagesperintervall, 5}.\n",
  file:write_file(FileName, InitConfig, [append])
.

%% clientLoop(ServerID, SendeIntervall, OutgoingMessagesCount, LifeTime) ->
%%   ServerID ! {messageClient,{getmsgid,node()}},
%%
%%   debugOutput(werkzeug:list2String(["clientLoop called: ServerID ",ServerID," SendeIntervall ",SendeIntervall," OutgoingMessagesCount ",OutgoingMessagesCount," LifeTime ",LifeTime]),""),
%%   timer:sleep(SendeIntervall),
%%
%%   receive
%%     %%%%%%%%%%%%%%%%%%%%%%%% GetMessages %%%%%%%%%%%%%%%%%%%%%%%%%
%%     {From,{reply,Number,Nachricht,Terminated}} -> 0;
%%
%%     %%%%%%%%%%%%%%%%%%%%%%% GetMessageID %%%%%%%%%%%%%%%%%%%%%%
%%     {From,{nid, Number}} ->
%%       Msg = lists:concat([node(),"-VSP/04-Gruppe 1- ID:",Number," Hier kann ihr Text stehn!  Sendezeit: ",werkzeug:timeMilliSecond()]),
%%       ServerID ! {messageClient,{dropmessage,{Number,Msg}}}
%%     ;
%%
%%
%%     %%%%%%%%%%%%%%%%%%%%%%% TIMEOUTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     {timeout,LifeTime_TimerRef,lifeTime} -> 0
%%   end
%% .



clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall) ->
  ServerID ! {messageClient,{getmsgid,node()}},
  debugOutput(lists:concat(["MessageID angefordert!,  ",werkzeug:timeMilliSecond()]),""),
  timer:sleep(SendeIntervall),
  receive
    {From,{nid, Number}} when OutgoingCount < MessagesPerIntervall ->
      Msg = lists:concat([node(),"-VSP/04-Gruppe 1- ID:",Number," Hier kann ihr Text stehn! Client OUT: ",werkzeug:timeMilliSecond()]),
      debugOutput(Msg,""),
      NewSendedMessages = werkzeug:pushSL(SendedMessages,{Number,{Number,Msg}}),
      ServerID ! {messageClient,{dropmessage,{Number,Msg}}},
      clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount+1,NewSendedMessages},LifeTime,MessagesPerIntervall);

    {From,{nid, Number}} when MessagesPerIntervall == OutgoingCount ->
        debugOutput(lists:concat([Number,"te_Nachricht um ",werkzeug:timeMilliSecond(), " vergessen zu senden ******"]),""),
        clientLoop(reader,ServerID,SendeIntervall,getNewSendeIntervall(SendeIntervall,CurrentSendeIntervall),{0,SendedMessages},LifeTime,MessagesPerIntervall);

    {timeout,LifeTime_TimerRef,lifeTime} ->
      debugOutput(lists:concat(["+++++ shutting client down ++++++++ ",werkzeug:timeMilliSecond() ]),"")
  end
;
clientLoop(reader,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall) ->
  ServerID ! {messageClient,{getmessages,node()}},
  debugOutput(lists:concat(["Message angefordert!,   ",werkzeug:timeMilliSecond()]),""),
  receive
    {From,{reply,ID, MSG,true}}  ->
      clientLoop(writer,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall)
  ;
    {From,{reply,ID, MSG,false}} ->
      case werkzeug:findSL(SendedMessages,ID) of
      %%% die eingegangene message ist nicht von diesem client gesendet wurden
        {-1,nok} ->
          debugOutput(MSG,"");
      %%% dieser client hat die message gesendet
        {ID,{ID,_Message}} ->
          debugOutput(lists:concat([MSG," +++++++++++ Client IN: ",werkzeug:timeMilliSecond()]),"");
        ANY -> debugOutput("case lol ... ",ANY)
      end,
      clientLoop(reader,ServerID,SendeIntervall,CurrentSendeIntervall,{OutgoingCount,SendedMessages},LifeTime,MessagesPerIntervall);


    {timeout,LifeTime_TimerRef,lifeTime} ->
      debugOutput(lists:concat(["+++++ shutting client down ++++++++ ",werkzeug:timeMilliSecond() ]),"")


  end

.

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



debugOutput(MSG,ANY) ->
  werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String([MSG,ANY]))
.