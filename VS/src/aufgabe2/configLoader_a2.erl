%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Okt 2013 18:31
%%%-------------------------------------------------------------------
-module(configLoader_a2).
-author("Sven").

%% API
-export([loadNodeConfig/1,createDefaultCoordinatorConfig/1]).


loadNodeConfig(ConfigFileName) ->
  ConfigFile = file:consult(ConfigFileName),
  case ConfigFile of
    {ok,ConfigList} ->
      NodeName = werkzeug:get_config_value(nodename,ConfigList),

      {ok,NN} = NodeName,
      [_Head| AdjacentNodes] = ConfigList,


      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok end,[NodeName]) of
        true ->
          {ok,{NN,AdjacentNodes}};
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

createDefaultCoordinatorConfig(FileName) ->

  InitConfig = "{arbeitszeit, 2}\n.{termzeit, 3}.\n{ggtprozessnummer, 3}.\n{nameservicenode, ns@Brummpa}.\n{nameservicename, nameservice}.\n{koordinatorname, chef}.\n{korrigieren, 0}.\n",
  file:write_file(FileName, InitConfig, [append])
.