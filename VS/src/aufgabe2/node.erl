%%%-------------------------------------------------------------------
%%% @author abg628
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Okt 2013 10:59
%%%-------------------------------------------------------------------
-module(node).
-author("abg628").

%% API
-export([start/0]).

start() ->
  {ok,DefaultConfigFile} = init:get_argument(configfilename),
  {ok,[[NodeNameServiceNode|_]|_]} = init:get_argument(nodenameservice),
  LogFileName = lists:concat([node(),".log"]),

  case configLoader_a2:loadNodeConfig(DefaultConfigFile) of
    {nok,configFileNotOK} ->       werkzeug:logging(LogFileName,"delete config file\n");
    {nok,fileNotFound} -> werkzeug:logging(LogFileName,"file not found\n");
    {ok,{NN,AdjacentNodes}}->
      werkzeug:logging(LogFileName,lists:concat(["+++ ",NN," started ",werkzeug:timeMilliSecond()," +++\n"])),

      case net_adm:ping(list_to_atom(NodeNameServiceNode)) of
        pang -> werkzeug:logging(LogFileName,"Nameservice not found\n");
        pong ->
            EdgeManagerADT = edgeManager:convertToEdgeManagerADT(AdjacentNodes),

            NodePID = spawn(fun() -> loop(NN,EdgeManagerADT,sleeping,LogFileName,0,"") end),
            global:register_name(NN,NodePID),
            NodePID
      end;

    {nok,ErrMsg} ->werkzeug:logging(LogFileName,ErrMsg)
  end
.

pingUntilPong(NodeName,X,pang)-> pingUntilPong(NodeName,X-1,net_adm:ping(list_to_atom(NodeName)));
pingUntilPong(_NodeName,0,pang)-> {fail};
pingUntilPong(_NodeName,_X,pong)-> {ok}.



loop(NodeName,EdgeManagerADT,State,Logfilename,FragmentLevel,FragmentID)->
  receive
    {initiate,0,FragName,NodeState,Edge} when State =:= sleeping -> 0,
          werkzeug:logging(Logfilename,lists:concat([werkzeug:timeMilliSecond()," initiate special case,",werkzeug:list2String([{initiate,0,FragName,NodeState,Edge}])," \n"])),
          AKMG = edgeManager:findNextBasic(EdgeManagerADT),
          case edgeManager:findNextBasic(EdgeManagerADT) of
          {ok,AKMG}-> edgeManager:markAs(EdgeManagerADT,AKMG,branch),
                  {Weight,{TargetNodeName,_Typ}} = AKMG,
                  Receiver = global:whereis_name(TargetNodeName),
                  werkzeug:logging(Logfilename,lists:concat([werkzeug:timeMilliSecond()," init 0 aufgerufen und connect versand, message: ",werkzeug:list2String([{connect,0,{Weight,NodeName,TargetNodeName}}])," \n"])),
                  Receiver ! {NodeName,{connect,0,{Weight,NodeName,TargetNodeName}}},
                  loop(NodeName,EdgeManagerADT,found,Logfilename,0,FragmentID);
           {nok,ANY} ->
             werkzeug:logging(Logfilename,lists:concat([werkzeug:timeMilliSecond(),"nix gefunden im EdgeManagerADT \n"]))
          end

    ;
    {initiate,Level,FragName,NodeState,Edge}  ->
          edgeManager:findNextBasic()

      ;
    {test,Level,FragName,Edge} -> 0;
    {accept,Edge} -> 0;
    {reject,Edge} -> 0;
    {report,Weight,Edge} -> 0;
    {changeroot,Edge} -> 0;
    {connect,Level,Edge} when Level =:= FragmentLevel -> 0;
    {connect,Level,Edge} when Level < FragmentLevel -> 0;
    {connect,Level,Edge} when Level > FragmentLevel -> 0;
    ANY ->
      werkzeug:logging(Logfilename,lists:concat([werkzeug:timeMilliSecond()," etwas empfangen",werkzeug:list2String([ANY]) ," \n"])),
      loop(NodeName,EdgeManagerADT,State,Logfilename)
  end
.


findMinimumEdge(AdjacentNodes) ->
  werkzeug:minNrSL(AdjacentNodes)
.
removeEdge(AdjacentNodes,Edge) ->
  lists:filter(fun(Item) -> Edge =/= Item  end,AdjacentNodes)
  .