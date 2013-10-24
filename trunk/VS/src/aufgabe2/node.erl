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
  {ok, DefaultConfigFile} = init:get_argument(configfilename),
  {ok, [[NodeNameServiceNode|_]|_]} = init:get_argument(nodenameservice),
  LogFileName = lists:concat([node(), ".log"]),

  case configLoader_a2:loadNodeConfig(DefaultConfigFile) of
    {nok, configFileNotOK} -> werkzeug:logging(LogFileName, "delete config file\n");
    {nok, fileNotFound} -> werkzeug:logging(LogFileName, "file not found\n");
    {ok, {NN, AdjacentNodes}} ->
      werkzeug:logging(LogFileName, lists:concat(["+++ ", NN, " started ", werkzeug:timeMilliSecond(), " +++\n"])),

      case net_adm:ping(list_to_atom(NodeNameServiceNode)) of
        pang -> werkzeug:logging(LogFileName, "Nameservice not found\n");
        pong ->
          EdgeManagerADT = edgeManager:convertToEdgeManagerADT(AdjacentNodes),

          NodePID = spawn(fun() -> loop(nodeManager:createNodeManagerADT(NN), EdgeManagerADT, LogFileName) end),
          global:register_name(NN, NodePID),
          NodePID
      end;

    {nok, ErrMsg} -> werkzeug:logging(LogFileName, ErrMsg)
  end
.

pingUntilPong(NodeName, X, pang) -> pingUntilPong(NodeName, X - 1, net_adm:ping(list_to_atom(NodeName)));
pingUntilPong(_NodeName, 0, pang) -> {fail};
pingUntilPong(_NodeName, _X, pong) -> {ok}.



loop(NodeManagerADT, EdgeManagerADT, Logfilename) ->
  receive
    {initiate, 0, FragName, NodeState, Edge} when nodeManager:isInState(NodeManagerADT, sleeping) -> 0,
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(), " initiate special case,", werkzeug:list2String([{initiate, 0, FragName, NodeState, Edge}]), " \n"])),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(), " init 0 aufgerufen und connect versand, message: ", werkzeug:list2String([{}]), " \n"])),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(), "nix gefunden im EdgeManagerADT \n"]))


  ;
    {initiate, Level, FragName, NodeState, Edge} ->
      edgeManager:findNextBasic()

  ;
    {test, Level, FragName, Edge} -> 0;
    {accept, Edge} -> 0;
    {reject, Edge} -> 0;
    {report, Weight, Edge} -> 0;
    {changeroot, Edge} -> 0;

    {connect, Level, Edge} when  nodeManager:isInState(NodeManagerADT, sleeping) ->
                        {NewNodeManagerADT, NewEdgeManagerADT} = wakeUp(NodeManagerADT,EdgeManagerADT,Logfilename),
                        loop(NewNodeManagerADT,NewEdgeManagerADT,Logfilename);

    {connect, Level,{Weight,Nodex,Nodey}} when Level < nodeManager:getLevel(NodeManagerADT) ->

                  NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT,{Weight,{edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),_}}),
                  messages:sendInitiate(nodeManager:getLevel(NodeManagerADT),nodeManager:getFragment(NodeManagerADT),nodeManager:getState(NodeManagerADT),{Weight,Nodex,Nodey}),
                  case nodeManager:isInState(NodeManagerADT,find) of
                    true -> NewNodeManagerADT = nodeManager:setFindCount(NodeManagerADT,nodeManager:getFindCount(NodeManagerADT)+1);
                    false -> NewNodeManagerADT = NodeManagerADT
                  end,
                 loop(NewNodeManagerADT,NewEdgeManagerADT,Logfilename);

    {connect, Level, {Weight,Nodex,Nodey}} when Level >= nodeManager:getLevel(NodeManagerADT) ->
                MyNodeName = nodeManager:getNodeName(NodeManagerADT),
                case edgeManager:isInState(EdgeManagerADT,{Weight,{edgeManager:getTargetNodeName({Nodex,Nodey},MyNodeName), _ }},basic) of
                  true -> messages:enqueue(MyNodeName,{connect,Level,{Weight,Nodex,Nodey}});

                  false -> messages:sendInitiate(nodeManager:getLevel(NodeManagerADT)+1,Weight,find,{Weight,Nodex,Nodey})

                end
    ;


    ANY ->
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(), " etwas empfangen", werkzeug:list2String([ANY]), " \n"])),
      loop(NodeManagerADT,EdgeManagerADT,Logfilename)

  end
.


wakeUp(NodeManagerADT, EdgeManagerADT,Logfilename) ->
  AKMG = edgeManager:findNextBasic(EdgeManagerADT),
  NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT, AKMG, branch),
  NewNodeManagerADT = nodeManager:initNodeManagerADT(NodeManagerADT),
  {Weight, {Nodename, State}} = AKMG,
  messages:sendConnect(0, {Weight, nodeManager:getNodeName(NodeManagerADT), Nodename}),
  {NewNodeManagerADT, NewEdgeManagerADT}
.

findMinimumEdge(AdjacentNodes) ->
  werkzeug:minNrSL(AdjacentNodes)
.
removeEdge(AdjacentNodes, Edge) ->
  lists:filter(fun(Item) -> Edge =/= Item end, AdjacentNodes)
.