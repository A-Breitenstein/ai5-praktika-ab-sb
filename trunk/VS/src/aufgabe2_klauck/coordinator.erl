%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Okt 2013 17:25
%%%-------------------------------------------------------------------
-module(coordinator).
-author("Sven").

%% API
-export([start/0]).


start() ->

  LogFileName = lists:concat([node(),".log"]),
  DefaultConfigFile = "koordinator.cfg",

  case configLoader:loadCoordinatorConfig(DefaultConfigFile) of
    {nok,configFileNotOK} ->       werkzeug:logging(LogFileName,"delete config file");
    {nok,fileNotFound} -> configLoader:createDefaultCoordinatorConfig(DefaultConfigFile),start();
    {ok,{AZ,TZ,GGTN,NSNode,NSName,KN,KO}}->
      werkzeug:logging(LogFileName,lists:concat(["+++ coordinator started ", werkzeug:timeMilliSecond()," +++"])),

      CoordinatorPid = spawn(fun() -> loop(initial,[],AZ,TZ,GGTN,NSNode,NSName,KN,KO,0) end),
      register(KN,CoordinatorPid),
      CoordinatorPid;
    {nok,ErrMsg} -> werkzeug:logging(LogFileName,ErrMsg)
  end
.

loop(initial,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount) ->
  receive
    {From,{getsteringval,StarterName}} ->
        {From,StarterName} ! {steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer},
        loop(initial,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount);

    {From,{helo,Clientname}} ->
        loop(initial,ggTNodeManager:registerGGTNode(GGTNodeList,{GGTNodeCount,Clientname}),ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount+1);
    {step} ->
        loop(bereit,ggTNodeManager:createNodeCircle(GGTNodeList),ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount);
    {kill} ->
        ggTNodeManager:killAllGGTNodes(GGTNodeList);
    {reset}->
        ggTNodeManager:killAllGGTNodes(GGTNodeList),
        loop(initial,[],ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount)

  end
;
loop(bereit,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount) ->
  receive
    {calc,WggT} ->
        ggTNodeManager:informGGTNodes(GGTNodeList,WggT);
    {brifmi,{Clientname,CMi,CZeit}} -> 0;
    {brifterm,{Clientname,CMi,CZeit},From} -> 0;
    {prompt} ->
        ggTNodeManager:promptGGTNodes(GGTNodeList),
        loop(bereit,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount);

    {nudge} ->
        ggTNodeManager:nudgeGGTNodes(GGTNodeList),
        loop(bereit,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount);
    {toggle} ->
        loop(bereit,GGTNodeList,ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,not(Koorigieren),GGTNodeCount);
    {kill} ->
        ggTNodeManager:killAllGGTNodes(GGTNodeList);
    {reset}->
        ggTNodeManager:killAllGGTNodes(GGTNodeList),
        loop(initial,[],ArbeitsZeit,TermZeit,GGTProzessnummer,NameServiceNode,NameServiceName,Koordinatorname,Koorigieren,GGTNodeCount)

end
.

