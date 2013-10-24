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
-export([createNodeManagerADT/1, initNodeManagerADT/1, isInState/2, getState/1, getFragment/1, getLevel/1, getBestWeight/1, getInBranch/1, getFindCount/1, getNodeName/1, setState/2, setFragment/2, setLevel/2, setBestWeight/2, setInBranch/2, setFindCount/2]).

%% NodeManagerADT::{State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName}
createNodeManagerADT(NodeName) -> {sleeping,null,null,null,null,null,NodeName}.

%%Init
initNodeManagerADT(NodeManagerADT) -> setState(setFindCount(setLevel(NodeManagerADT, 0),0),found).

%%IsInState(NodeManagerADT, State)-> boolean; State = {sleeping | find | found}
isInState(NodeManagerADT, State) -> getState(NodeManagerADT) =:= State.


%GETTER----------------

%% getter State
  getState(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, State.

%% getter Fragment
getFragment(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, Fragment.

%% getter Level
getLevel(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, Level.

%% getter BestWeight
getBestWeight(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, BestWeight.

%% getter InBranch
getInBranch(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, InBranch.

%% getter FindCount
getFindCount(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, FindCount.

%% getter NodeName
getNodeName(NodeManagerADT) -> {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT, NodeName.


%SETTER---------------------------

%% setter State
setState(NodeManagerADT, NewState) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {NewState,Fragment,Level,BestWeight,InBranch,FindCount,NodeName}
.

%% setter Fragment
setFragment(NodeManagerADT, NewFragment) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {State,NewFragment,Level,BestWeight,InBranch,FindCount,NodeName}
.

%% setter Level
setLevel(NodeManagerADT, NewLevel) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {State,Fragment,NewLevel,BestWeight,InBranch,FindCount,NodeName}
.

%% setter BestWeight
setBestWeight(NodeManagerADT, NewBestWeight) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {State,Fragment,Level,NewBestWeight,InBranch,FindCount,NodeName}
.

%% setter InBranch
setInBranch(NodeManagerADT, NewInBranch) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName}
.

%% setter FindCount
setFindCount(NodeManagerADT, NewFindCount) ->
  {State,Fragment,Level,BestWeight,InBranch,FindCount,NodeName} = NodeManagerADT,
  {State,Fragment,Level,BestWeight,InBranch,NewFindCount,NodeName}
.