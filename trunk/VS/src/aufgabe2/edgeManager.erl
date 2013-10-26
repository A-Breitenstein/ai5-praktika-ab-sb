%%%-------------------------------------------------------------------
%%% @author abg628
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Okt 2013 13:36
%%%-------------------------------------------------------------------
-module(edgeManager).
-author("abg628").

%% API
-export([convertToEdgeManagerADT/1, findNextBasic/1, setEdgeState/3, availableOfState/2, isInState/3, findNext/2, getTargetNodeName/2, getAllBranchEdgesExceptThisOne/2]).

%%EdgeManagerADT :: {[{Weight,{Nodename,basic}}]}
%% [{7,{node7,basic}},{8,{node8,basic}},{2,{node2,basic}},{4,{node4,basic}}]
convertToEdgeManagerADT(AdjacentNodes) ->
  ExtendedAdjacentNodes = lists:foldl(fun({Weight, Nodename}, Accu) -> werkzeug:pushSL(Accu, {Weight, {Nodename, basic}}) end, [], AdjacentNodes),
  {ExtendedAdjacentNodes}
.

findNextBasic(EdgeManagerADT) ->
  findNext(EdgeManagerADT, basic)
.
%%findNext(EdgeManagerADT, State) ->? {ok, Edge} | {nok, []}; State={basic | rejected | branch}
findNext(EdgeManagerADT, State) ->
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  FilterNodes = lists:filter(fun({AccWeight, {AccNodename, AccTyp}}) -> AccTyp =:= State end, ExtendedAdjacentNodes),
  case werkzeug:lengthSL(FilterNodes) > 0 of
    true ->
      [FirstElem|RestFilterNodes] = FilterNodes,
      {ok, lists:foldl(fun({Weight, {Nodename, Typ}}, {AccWeight, {AccNodename, AccTyp}}) ->
        case {Typ =:= State andalso Weight < AccWeight} of
          {true} -> {Weight, {Nodename, Typ}};
          {false} -> {AccWeight, {AccNodename, AccTyp}}
        end

      end, FirstElem, FilterNodes)};
    false -> {nok, []}
  end
.
setEdgeState(EdgeManagerADT, Edge, Typ) ->
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  {Weight, {NodeName, _}} = Edge,
  {werkzeug:pushSL(lists:filter(fun({ItemWeight, {ItemNodeName, _Typ}}) ->
    ItemNodeName =/= NodeName
  end, ExtendedAdjacentNodes), {Weight, {NodeName, Typ}})}
.

getTargetNodeName({Nodex,Nodey},NodeName) ->
  case NodeName == Nodex of
    true ->  Nodey;
    false ->  Nodex
  end
.
%%isState(EdgeManagerADT, Edge, State) -> boolean; State={basic | rejected | branch}
isInState(EdgeManagerADT, Edge, State) ->
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  {Weight, {NodeName, TargetState}} = Edge,
  FilterNodes = lists:filter(fun({AccWeight, {AccNodename, AccTyp}}) -> AccWeight == Weight andalso AccNodename == NodeName andalso AccTyp =:= State end, ExtendedAdjacentNodes),

  werkzeug:lengthSL(FilterNodes) =:= 1
  .
%%availableOfState(EdgeManagerADT, State) -> boolean; State={basic | rejected | branch}
availableOfState(EdgeManagerADT, TargetState) ->
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  lists:any(fun({_Weight, {_Nodename, ItemState}}) -> ItemState =:= TargetState end, ExtendedAdjacentNodes)
.

getAllBranchEdgesExceptThisOne(EdgeManagerADT,Edge) ->
  {Weight,{TargetNodeName,_}} = Edge,
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  lists:filter(fun({ItemWeight,ItemTargetName,ItemState})->
                TargetNodeName =/= ItemTargetName andalso ItemState == branch
               end,ExtendedAdjacentNodes)
.
toString(EdgeManagerADT) ->
  {ExtendedAdjacentNodes} = EdgeManagerADT,
  werkzeug:list2String(ExtendedAdjacentNodes)
.


