%%%-------------------------------------------------------------------
%%% @author abg628
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Okt 2013 10:21
%%%-------------------------------------------------------------------
-module(nodeManager).
-author("abg628").

%% API
-export([createNodeManagerADT/1, initNodeManagerADT/1, isInState/2, getState/1, getFragment/1, getLevel/1, getBestWeight/1, getInBranch/1, getFindCount/1, getNodeName/1, setState/2, setFragment/2, setLevel/2, setBestWeight/2, setInBranch/2, setFindCount/2, getBestEdge/1, getTestEdge/1, setBestEdge/2, setTestEdge/2, toString/1]).

%% NodeManagerADT::{State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge}
createNodeManagerADT(NodeName) -> {sleeping,null,null,null,null,null,NodeName,null,null}.

%%Init
initNodeManagerADT(NodeManagerADT) -> setState(setFindCount(setLevel(NodeManagerADT, 0),0),found).

%%IsInState(NodeManagerADT, State)-> boolean; State = {sleeping | find | found}
isInState(NodeManagerADT, State) -> getState(NodeManagerADT) =:= State.

toString(NodeManagerADT) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  String1 = werkzeug:list2String([InBranch]),
  String2 = werkzeug:list2String([BestEdge]),
  String3 = werkzeug:list2String([TestEdge]),
  lists:concat(["NodeManagerADT on Node ",NodeName," currentState ",State," FragmentID ",Fragment," Level ",Level," BestWeight ",BestWeight," Findcount ",FindCount," BestEdge ",String2," InBranch ",String1," TestEdge ",String3])
.

%GETTER----------------

%% getter State
  getState(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, State.

%% getter Fragment
getFragment(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, Fragment.

%% getter Level
getLevel(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, Level.

%% getter BestWeight
getBestWeight(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, BestWeight.

%% getter InBranch
getInBranch(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, InBranch.

%% getter FindCount
getFindCount(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, FindCount.

%% getter NodeName
getNodeName(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, NodeName.

%% getter BestEdge
getBestEdge(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, BestEdge.

%% getter TestEdge
getTestEdge(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT, TestEdge.


%SETTER---------------------------

%% setter State
setState(NodeManagerADT, NewState) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {NewState,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge}
.

%% setter Fragment
setFragment(NodeManagerADT, NewFragment) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,NewFragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge}
.

%% setter Level
setLevel(NodeManagerADT, NewLevel) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,NewLevel,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge}
.

%% setter BestWeight
setBestWeight(NodeManagerADT, NewBestWeight) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,Level,NewBestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge}
.

%% setter InBranch
setInBranch(NodeManagerADT, NewInBranch) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,NewInBranch,FindCount,NodeName,BestEdge,TestEdge}
.

%% setter FindCount
setFindCount(NodeManagerADT, NewFindCount) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,InBranch,NewFindCount,NodeName,BestEdge,TestEdge}
.

%% setter BestEdge
setBestEdge(NodeManagerADT, NewBestEdge) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,NewBestEdge,TestEdge}
.
%% setter TestEdge
setTestEdge(NodeManagerADT, NewTestEdge) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,TestEdge} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName,BestEdge,NewTestEdge}
.