%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Okt 2013 20:40
%%%-------------------------------------------------------------------
-module(configFileLoader).
-author("Sven").

%% API
-export([defaultServerSetup/1,loadServerConfig/1,defaultClientSetup/1,loadClientConfig/1]).


defaultServerSetup(FileName) ->
  InitConfig = "{serverTimeout, 400000}.\n{clientTimeout, 16000}.\n{maxMessageCount, 7}.\n{processName, 'wk'}.\n",
  file:write_file(FileName, InitConfig, [append])
.
defaultClientSetup(FileName) ->

  InitConfig =  "{clients, 3}.\n{lifetime, 90000}.\n{serverid, {wk,'messageServer@Sven-PC'}}.\n{sendeintervall, 5000}.\n{messagesperintervall, 5}.\n",
  file:write_file(FileName, InitConfig, [append])
.

loadServerConfig(DefaultConfigFile) ->
  ConfigFile = file:consult(DefaultConfigFile),
  case ConfigFile of
    {ok,ConfigList} ->
      ServerTimeout = werkzeug:get_config_value(serverTimeout,ConfigList),
      ClientTimeout = werkzeug:get_config_value(clientTimeout,ConfigList),
      MaxMessageCount = werkzeug:get_config_value(maxMessageCount,ConfigList),
      ProcessName = werkzeug:get_config_value(processName,ConfigList),

      {ok,ST} = ServerTimeout,
      {ok,CT} = ClientTimeout,
      {ok,MMC} = MaxMessageCount,
      {ok,PN} = ProcessName,

      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok  end,[ServerTimeout,ClientTimeout,MaxMessageCount,ProcessName]) of
        true ->
          {ok,{ST,CT,MMC,PN}};
        false ->
          {nok,configFileNotOK}
      end;

  %% enoent seams to be the file not found / exsists error, see file:consult(...) docu
    {error,enoent} ->
         {nok,fileNotFound};
    {error,ErrMsg}->
          {nok,ErrMsg}
  end
.




createNumList(0, List) ->
  List
;
createNumList(ClientsCount, List) ->
      createNumList(ClientsCount-1,[ClientsCount,List])
.

loadClientConfig(DefaultConfigFile) ->
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
      {ok,ClientsCount} = Clients,

      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok  end,[LifeTime,Server,SendeIntervall,MessagesIntervall,Clients]) of
        true ->
          {ok,{LT,ServerID,SI,MI,ClientsCount}};
        false ->
          {nok,configFileNotOK}
      end;

  %% enoent seams to be the file not found / exsists error, see file:consult(...) docu
    {error,enoent} ->
      {nok,fileNotFound};
    {error,ErrMsg}->
      {nok,ErrMsg}
  end
.