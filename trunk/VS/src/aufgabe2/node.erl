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
%%
%% pingUntilPong(NodeName, X, pang) -> pingUntilPong(NodeName, X - 1, net_adm:ping(list_to_atom(NodeName)));
%% pingUntilPong(_NodeName, 0, pang) -> {fail};
%% pingUntilPong(_NodeName, _X, pong) -> {ok}.



loop(NodeManagerADT, EdgeManagerADT, Logfilename) ->
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"NodeManagerADT: ",nodeManager:toString(NodeManagerADT) ,"\n"])),
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"EdgeManagerADT: ",edgeManager:toString(EdgeManagerADT) ,"\n"])),
  IS_SLEEPING = nodeManager:isInState(NodeManagerADT, sleeping),
  NODE_LEVEL = nodeManager:getLevel(NodeManagerADT),
  receive
    {initiate, Level, FragName, NodeState, Edge} ->
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{initiate, Level, FragName, NodeState, Edge} ]) ,"\n"])),

      {Weight,Nodex,Nodey} = Edge,
      NewNodeManagerADT = nodeManager:setBestEdge(
                            nodeManager:setBestWeight(
                                nodeManager:setInBranch(
                                    nodeManager:setState(
                                        nodeManager:setFragment(
                                              nodeManager:setLevel(NodeManagerADT,
                                              Level),
                                        FragName),
                                    NodeState),
                                Edge),
                            1000000),
                        null),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"NodeManagerADT Updated: ",nodeManager:toString(NewNodeManagerADT) ,"\n"])),

      MyNodeName = nodeManager:getNodeName(NewNodeManagerADT),
      TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},MyNodeName),
      NewFindCount =lists:foldl(fun({ItemWeight,ItemTargetNodeName,_},Accu)->
                          messages:sendInitiate(TargetNodeName,Level,FragName,NodeState,{ItemWeight,MyNodeName,ItemTargetNodeName}),
                          case NodeState == find of
                            true-> Accu +1;
                            false-> Accu
                          end
                                      end,
                    nodeManager:getFindCount(NewNodeManagerADT),
                    edgeManager:getAllBranchEdgesExceptThisOne(EdgeManagerADT,{Weight,{TargetNodeName,""}})),
      NewNewNodeMangerADT = nodeManager:setFindCount(NewNodeManagerADT,NewFindCount),
      case NodeState == find of
        true -> {NewNewNewNodeManagerADT,NewEdgeMangerADT} = test(NewNewNodeMangerADT,EdgeManagerADT,Logfilename);
        false -> NewNewNewNodeManagerADT = NewNewNodeMangerADT,
                  NewEdgeMangerADT = EdgeManagerADT
      end,
      loop(NewNewNewNodeManagerADT,NewEdgeMangerADT,Logfilename);






    {test, Level, FragName, Edge} when IS_SLEEPING ->
                werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{test, Level, FragName, Edge} ]) ," in sleeping state \n"])),
                {NewNodeManagerADT, NewEdgeManagerADT} = wakeUp(NodeManagerADT,EdgeManagerADT,Logfilename),
                loop(NewNodeManagerADT,NewEdgeManagerADT,Logfilename);


    {test,Level,FragName,Edge} when Level > NODE_LEVEL ->
                werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{test, Level, FragName, Edge} ]) ," with Level > NodeLevel \n"])),
                messages:enqueue(nodeManager:getNodeName(NodeManagerADT),{test,Level,FragName,Edge}),
                werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"placeing received message on end of queue: ", werkzeug:list2String([{test, Level, FragName, Edge} ]) ," \n"])),
                loop(NodeManagerADT,EdgeManagerADT,Logfilename);


    {test,Level,FragName,{Weight,Nodex,Nodey}} when Level =< NODE_LEVEL ->
              werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{test, Level, FragName, {Weight,Nodex,Nodey}} ]) ," with Level <= NodeLevel \n"])),

              TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),
              case FragName =/= nodeManager:getFragment(NodeManagerADT) of
                true -> messages:sendAccept(TargetNodeName,{Weight,Nodex,Nodey}),
                        werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Accept sended to: ",TargetNodeName," with Edge ",{Weight,Nodex,Nodey},"\n"])),
                        NewNodeManagerADT = NodeManagerADT,
                        NewNewEdgeManagerADT = EdgeManagerADT;

                false -> case edgeManager:isInState(EdgeManagerADT,{Weight,{TargetNodeName,""}},basic) of
                            true ->
                              werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Edge marked as rejected ! ",{Weight,TargetNodeName},"\n"])),
                              NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT,{Weight,{TargetNodeName,""}},rejected);
                            false -> NewEdgeManagerADT = EdgeManagerADT
                         end,

                        {TestEdgeWeight,TestEdgeNodeName,_} = nodeManager:getTestEdge(NodeManagerADT),
                        case TestEdgeNodeName =/= TargetNodeName andalso TestEdgeWeight =/= Weight of
                          true -> messages:sendReject(TargetNodeName,{Weight,Nodex,Nodey}),
                                  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Rejected sended to: ",TargetNodeName, " on edge",{Weight,Nodex,Nodey},"\n"])),
                                  NewNodeManagerADT = NodeManagerADT,
                                  NewNewEdgeManagerADT = NewEdgeManagerADT;
                          false-> {NewNodeManagerADT,NewNewEdgeManagerADT} = test(NodeManagerADT,NewEdgeManagerADT,Logfilename)
                        end

              end,

              loop(NewNodeManagerADT,NewNewEdgeManagerADT,Logfilename);



    {accept, {Weight,Nodex,Nodey}} ->
                                      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{accept, {Weight,Nodex,Nodey}}]) ,"\n"])),

                                      NewNodeManagerADT =nodeManager:setTestEdge(NodeManagerADT,null),
                                      case Weight < nodeManager:getBestWeight(NewNodeManagerADT) of
                                        true ->
                                                werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"setBestEdge and BestWeight : ", werkzeug:list2String([{Weight,Nodex,Nodey}]) ,"\n"])),

                                                 NewNewNodeManagerADT = nodeManager:setBestEdge(
                                                                              nodeManager:setBestWeight(NodeManagerADT,
                                                                              Weight),
                                                                         {Weight,Nodex,Nodey});
                                        false -> NewNewNodeManagerADT = NewNodeManagerADT
                                      end,
                                      loop(report(NewNewNodeManagerADT,Logfilename),EdgeManagerADT,Logfilename);



    {reject, {Weight,Nodex,Nodey}} ->
            werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{reject, {Weight,Nodex,Nodey}}]) ,"\n"])),

             TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),
             case edgeManager:isInState(EdgeManagerADT,{Weight,{TargetNodeName,""}},basic) of
               true->
                       werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Edge marked as rejected ! ",{Weight,TargetNodeName},"\n"])),
                       NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT,{Weight,{TargetNodeName,""}},rejected);
               false-> NewEdgeManagerADT = EdgeManagerADT
             end,
             {NewNodeManagerADT,NewNewEdgeManagerADT} = test(NodeManagerADT,NewEdgeManagerADT,Logfilename),
             loop(NodeManagerADT,NewNewEdgeManagerADT,Logfilename);

    {report, ReportedWeight, {Weight,Nodex,Nodey}} ->
              werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{report, ReportedWeight, {Weight,Nodex,Nodey}}]) ,"\n"])),
              {InBranchWeight,InBranchNodex,InBranchNodey} = nodeManager:getInBranch(NodeManagerADT),
              InBranchTargetName = edgeManager:getTargetNodeName({InBranchNodex,InBranchNodey},nodeManager:getNodeName(NodeManagerADT)),
              TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),
              case InBranchWeight =/= Weight andalso InBranchTargetName =/= TargetNodeName of
                true->  NewNodeManagerADT = nodeManager:setFindCount(NodeManagerADT,nodeManager:getFindCount(NodeManagerADT)-1),
                        case ReportedWeight < nodeManager:getBestWeight(NewNodeManagerADT) of
                          true ->
                            werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"setBestEdge and BestWeight : ", werkzeug:list2String([{ReportedWeight,Nodex,Nodey}]) ,"\n"])),
                            NewNewNodeManagerADT = nodeManager:setBestEdge(
                                                        nodeManager:setBestWeight(NewNodeManagerADT,
                                                        ReportedWeight),
                                                   {Weight,Nodex,Nodey});
                          false -> NewNewNodeManagerADT = NewNodeManagerADT
                        end,

                        loop(report(NewNewNodeManagerADT,Logfilename),EdgeManagerADT,Logfilename);
                false ->
                        case nodeManager:isInState(NodeManagerADT,find) of
                               true->
                                 messages:enqueue(nodeManager:getNodeName(NodeManagerADT),{report,ReportedWeight,{Weight,Nodex,Nodey}}),
                                 werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Report message placed at the end of queue ", werkzeug:list2String([{report,ReportedWeight,{Weight,Nodex,Nodey}}]) ,"\n"])),
                                 loop(NodeManagerADT,EdgeManagerADT,Logfilename);
                                false-> BestWeight = nodeManager:getBestWeight(NodeManagerADT),
                                      case BestWeight < ReportedWeight of
                                          true-> NewEdgeManagerADT = changeRoot(NodeManagerADT,EdgeManagerADT,Logfilename),
                                                 loop(NodeManagerADT,NewEdgeManagerADT,Logfilename);
                                          false when BestWeight==ReportedWeight andalso BestWeight == 1000000  ->
                                            0,
                                            werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"++++++++++++ HALT +++++++ BestWeight and ReportedWeight: ",BestWeight, werkzeug:list2String([{report,ReportedWeight,{Weight,Nodex,Nodey}}]) ,"\n"]))
                                          %% halt...
                                        end

                        end

              end;
    {changeroot, Edge} ->
            werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{changeroot, Edge}]) ,"\n"])),
            loop(NodeManagerADT ,changeRoot(NodeManagerADT,EdgeManagerADT,Logfilename),Logfilename);



    {connect, Level, Edge} when  IS_SLEEPING ->
                        werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{connect, Level, Edge}]) ," in sleeping state \n"])),
                        {NewNodeManagerADT, NewEdgeManagerADT} = wakeUp(NodeManagerADT,EdgeManagerADT,Logfilename),
                        loop(NewNodeManagerADT,NewEdgeManagerADT,Logfilename);



    {connect, Level,{Weight,Nodex,Nodey}} when Level < NODE_LEVEL ->
                  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{connect, Level,{Weight,Nodex,Nodey}}]) ," with Level < NodeLevel \n"])),
                  TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),
                  NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT,{Weight,{TargetNodeName,""}},branch),
                  messages:sendInitiate(TargetNodeName,nodeManager:getLevel(NodeManagerADT),nodeManager:getFragment(NodeManagerADT),nodeManager:getState(NodeManagerADT),{Weight,Nodex,Nodey}),
                  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"initiate sended to: ",TargetNodeName," with level ",nodeManager:getLevel(NodeManagerADT)," fragmentid ",nodeManager:getFragment(NodeManagerADT)," state ",nodeManager:getState(NodeManagerADT)," edge ", werkzeug:list2String([{Weight,Nodex,Nodey}]) ,"\n"])),
                  case nodeManager:isInState(NodeManagerADT,find) of
                    true ->
                      NewNodeManagerADT = nodeManager:setFindCount(NodeManagerADT,nodeManager:getFindCount(NodeManagerADT)+1),
                      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"findcount updated: ",nodeManager:getFindCount(NodeManagerADT),"\n"]));
                      false -> NewNodeManagerADT = NodeManagerADT
                  end,
                 loop(NewNodeManagerADT,NewEdgeManagerADT,Logfilename);

    {connect, Level, {Weight,Nodex,Nodey}} when Level >= NODE_LEVEL ->
                werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"received message: ", werkzeug:list2String([{connect, Level,{Weight,Nodex,Nodey}}]) ," with Level >= NodeLevel \n"])),

                MyNodeName = nodeManager:getNodeName(NodeManagerADT),
                TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},MyNodeName),
                case edgeManager:isInState(EdgeManagerADT,{Weight,{TargetNodeName, "" }},basic) of
                  true -> messages:enqueue(MyNodeName,{connect,Level,{Weight,Nodex,Nodey}}),
                          werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"Connect message placed at the end of queue ", werkzeug:list2String([{connect,Level,{Weight,Nodex,Nodey}}]) ,"\n"]));

                  false ->
                          messages:sendInitiate(TargetNodeName,nodeManager:getLevel(NodeManagerADT)+1,Weight,find,{Weight,Nodex,Nodey}),
                          werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"initiate sended to: ",TargetNodeName," with level ",nodeManager:getLevel(NodeManagerADT)+1," fragmentid ",Weight," state ",find," edge ", werkzeug:list2String([{Weight,Nodex,Nodey}]) ,"\n"]))
                end,
                loop(NodeManagerADT,EdgeManagerADT,Logfilename);


    ANY ->
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(), "received anything:  ", werkzeug:list2String([ANY]), " \n"])),
      loop(NodeManagerADT,EdgeManagerADT,Logfilename)

  end
.

changeRoot(NodeManagerADT,EdgeManagerADT,Logfilename) ->
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"changeroot called: \n"])),

  {Weight,Nodex,Nodey} = nodeManager:getBestEdge(NodeManagerADT),
  MyNodeName = nodeManager:getNodeName(NodeManagerADT),
  TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},MyNodeName),
  case edgeManager:isInState(EdgeManagerADT,{Weight,{TargetNodeName,""}},branch) of
    true-> messages:sendChangeRoot(TargetNodeName,{Weight,Nodex,Nodey}),
           werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"changeroot sended to: ",TargetNodeName," ", werkzeug:list2String([{changeroot,{Weight,Nodex,Nodey}}]) ,"\n"])),
           NewEdgeManagerADT = EdgeManagerADT;

    false-> messages:sendConnect(TargetNodeName,nodeManager:getLevel(NodeManagerADT),{Weight,Nodex,Nodey}),
            werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"connect sended to: ",TargetNodeName," ", werkzeug:list2String([{connect,nodeManager:getLevel(NodeManagerADT),{Weight,Nodex,Nodey}}]) ,"\n"])),
            NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT,{Weight,{TargetNodeName,""}},branch)
  end,
  NewEdgeManagerADT
.



report(NodeManagerADT,Logfilename) ->
%%   werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"report called: \n"])),
  case nodeManager:getFindCount(NodeManagerADT) == 0 andalso nodeManager:getTestEdge(NodeManagerADT) == null of
    true ->
      InBranchEgde = nodeManager:getInBranch(NodeManagerADT),
      {Weight,Nodex,Nodey} = InBranchEgde,
      TargetNodeName = edgeManager:getTargetNodeName({Nodex,Nodey},nodeManager:getNodeName(NodeManagerADT)),
      messages:sendReport(TargetNodeName,nodeManager:getBestWeight(NodeManagerADT),InBranchEgde),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"test sended to: ",TargetNodeName," ", werkzeug:list2String([nodeManager:getBestWeight(NodeManagerADT),InBranchEgde]) ,"\n"])),
      nodeManager:setState(NodeManagerADT,found);
    false -> NodeManagerADT
  end
.

test(NodeMangerADT, EdgeManagerADT,Logfilename) ->
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"test called: \n"])),
  case edgeManager:availableOfState(EdgeManagerADT,basic) of
    true -> {ok,TestEdge} = edgeManager:findNextBasic(EdgeManagerADT),
      NewNodeManagerADT = nodeManager:setTestEdge(NodeMangerADT,TestEdge),
      {Weight,{TargetNodeName,_}} = TestEdge,
      messages:sendTest(TargetNodeName,
              nodeManager:getLevel(NewNodeManagerADT),
              nodeManager:getFragment(NewNodeManagerADT),
              {Weight,nodeManager:getNodeName(NewNodeManagerADT),TargetNodeName}),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"test sended to: ",TargetNodeName," ", werkzeug:list2String([nodeManager:getLevel(NewNodeManagerADT),nodeManager:getFragment(NewNodeManagerADT),{Weight,nodeManager:getNodeName(NewNodeManagerADT),TargetNodeName}]) ,"\n"])),
      {NewNodeManagerADT,EdgeManagerADT};

    false ->
      NewNodeManagerADT = nodeManager:setTestEdge(NodeMangerADT,null),
      werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"testedge reseted \n"])),
      {report(NewNodeManagerADT,Logfilename),EdgeManagerADT}
  end
.

wakeUp(NodeManagerADT, EdgeManagerADT,Logfilename) ->
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"wakeup called: \n"])),
  {ok,AKMG} = edgeManager:findNextBasic(EdgeManagerADT),
  NewEdgeManagerADT = edgeManager:setEdgeState(EdgeManagerADT, AKMG, branch),
  NewNodeManagerADT = nodeManager:initNodeManagerADT(NodeManagerADT),
  {Weight, {TargetNodeName, State}} = AKMG,
  messages:sendConnect(TargetNodeName,0, {Weight, nodeManager:getNodeName(NodeManagerADT), TargetNodeName}),
  werkzeug:logging(Logfilename, lists:concat([werkzeug:timeMilliSecond(),"connect sended to: ",TargetNodeName," ", werkzeug:list2String([0, {Weight, nodeManager:getNodeName(NodeManagerADT), TargetNodeName}]) ,"\n"])),
  {NewNodeManagerADT, NewEdgeManagerADT}
.
