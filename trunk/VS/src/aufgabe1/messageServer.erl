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

doStartUp() ->
  ServerPid = spawn(fun() -> doStartUp([]) end),
    register(tipOfTheDaymessageServer,ServerPid),
  ServerPid.
.
doStartUp(Args) ->
  DefaultConfigFile = "serverconfig.cfg",
  ConfigFile = file:consult(DefaultConfigFile),
  case ConfigFile of
    {ok,ConfigList} ->
      ServerTimeout = get_config_value(serverTimeout,ConfigList),
      ClientTimeout = get_config_value(clientTimeout,ConfigList),
      MaxMessageCount = get_config_value(maxMessageCount,ConfigList),

      debugOutput("ConfigList",ConfigList),
      debugOutput("ServerTimeout",ServerTimeout),
      debugOutput("ClientTimeout",ClientTimeout),
      debugOutput("MaxMessageCount",MaxMessageCount),

      {ok,ST} = ServerTimeout,
      {ok,CT} = ClientTimeout,
      {ok,MMC} = MaxMessageCount,
      case true of
        true ->
          serverLoop([{serverTimeout,ST},{clientTimeout,CT},{maxMessageCount,MMC}]);
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
get_config_value(_Key, []) ->
  {error, not_found};
get_config_value(Key, [{Key, Value} | _Config]) ->
  {ok, Value};
get_config_value(Key, [{_Other, _Value} | Config]) ->
  get_config_value(Key, Config).

checkValueAvailable(List) ->
 0
;
checkValueAvailable([]) ->
 0
.
serverLoop(ServerState) ->
  receive
    {From,{getMsgId,RechnerID}} ->
      debugOutput("getMsgId called",From),
      serverLoop(ServerState);

    {From,{dropMsg,SenderID, Zeit, Nachricht,MessageID}}->
      debugOutput("dropMsg called",From),
      serverLoop(ServerState);

    {From,{getMsg,RechnerID}} ->
      debugOutput("getMsg called",From),
      serverLoop(ServerState)
  end
.
debugOutput(MSG,ANY) ->
  io:format("debug output: ~p :: ~p\n", [MSG,ANY])
.