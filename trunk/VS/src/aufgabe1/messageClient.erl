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


      {ok,LT} = LifeTime,
      {ok,ServerID} = Server,
      {ok,SI} = SendeIntervall,
      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok end,[LifeTime,Server,SendeIntervall]) of
        true ->
          LifeTime_TimerRef = erlang:start_timer(LT,self(),lifeTime),
          werkzeug:logging(LogFileName,lists:concat(["+++ client started ",werkzeug:timeMilliSecond()," +++"])),

          clientLoop(ServerID,SI,0,LT);

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

  InitConfig =  "{clients, 9}.\n{lifetime, 90000}.\n{serverid, {tipOfTheDaymessageServer,'messageServer@Sven-PC'}}.\n{sendeintervall, 5000}.\n",
  file:write_file(FileName, InitConfig, [append])
.

clientLoop(ServerID, SendeIntervall, OutgoingMessagesCount, LifeTime) ->
  ServerID ! {messageClient,{getmsgid,node()}},

  debugOutput(werkzeug:list2String(["clientLoop called: ServerID ",ServerID," SendeIntervall ",SendeIntervall," OutgoingMessagesCount ",OutgoingMessagesCount," LifeTime ",LifeTime]),""),
  timer:sleep(SendeIntervall),

  receive
    %%%%%%%%%%%%%%%%%%%%%%%% GetMessages %%%%%%%%%%%%%%%%%%%%%%%%%
    {From,{reply,Number,Nachricht,Terminated}} -> 0;

    %%%%%%%%%%%%%%%%%%%%%%% GetMessageID %%%%%%%%%%%%%%%%%%%%%%
    {From,{nid, Number}} ->
      Msg = lists:concat([node(),"-VSP/04-Gruppe 1- ID:",Number," Hier kann ihr Text stehn!  Sendezeit: ",werkzeug:timeMilliSecond()]),
      ServerID ! {messageClient,{dropmessage,{Number,Msg}}}
    ;


    %%%%%%%%%%%%%%%%%%%%%%% TIMEOUTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {timeout,LifeTime_TimerRef,lifeTime} -> 0
  end
.

debugOutput(MSG,ANY) ->
  werkzeug:logging(lists:concat([node(),".log"]),werkzeug:list2String([MSG,ANY]))
.