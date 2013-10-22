%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Okt 2013 18:31
%%%-------------------------------------------------------------------
-module(configLoader).
-author("Sven").

%% API
-export([loadCoordinatorConfig/1,createDefaultCoordinatorConfig/1]).


loadCoordinatorConfig(ConfigFileName) ->
  ConfigFile = file:consult(ConfigFileName),
  case ConfigFile of
    {ok,ConfigList} ->
      ArbeitsZeit = werkzeug:get_config_value(arbeitszeit,ConfigList),
      Termzeit = werkzeug:get_config_value(termzeit,ConfigList),
      GGTProzessnummer = werkzeug:get_config_value(ggtprozessnummer,ConfigList),
      NameServiceNode = werkzeug:get_config_value(nameservicenode,ConfigList),
      NameServiceName = werkzeug:get_config_value(nameservicename,ConfigList),
      Koordinatorname = werkzeug:get_config_value(koordinatorname,ConfigList),
      Korrigieren = werkzeug:get_config_value(korrigieren,ConfigList),

      {ok,AZ} = ArbeitsZeit,
      {ok,TZ} = Termzeit,
      {ok,GGTN} = GGTProzessnummer,
      {ok,NSNode} = NameServiceNode,
      {ok,NSName} = NameServiceName,
      {ok,KN} = Koordinatorname,
      {ok,KO} = Korrigieren,

      %% sind alle eingelesenen werte OK ?
      case lists:all(fun(Item)-> {OK,_Val} = Item,OK =:= ok end,[ArbeitsZeit,Termzeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Korrigieren]) of
        true ->
          {ok,{AZ,TZ,GGTN,NSNode,NSName,KN,KO}};
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