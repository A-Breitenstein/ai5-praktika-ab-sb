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
-export([convertToEdgeManagerADT/1, findNextBasic/1, markAs/3]).

%%EdgeManagerADT :: {LastMarkedBranchWeight,[{Weight,{Nodename,basic}}]}
%% [{7,{node7,basic}},{8,{node8,basic}},{2,{node2,basic}},{4,{node4,basic}}]
findNextBasic(EdgeManagerADT) ->
  findNext(EdgeManagerADT, basic)
.
findNext(EdgeManagerADT, OfTyp) ->
  {LastMarkedBranchWeight, ExtendedAdjacentNodes} = EdgeManagerADT,
  FilterNodes = lists:filter(fun( {AccWeight, {AccNodename, AccTyp}})-> AccTyp =:= OfTyp end,ExtendedAdjacentNodes),
  case werkzeug:lengthSL(FilterNodes) > 0 of
    true ->
              [FirstElem|RestFilterNodes] = FilterNodes,
      {ok,lists:foldl(fun({Weight, {Nodename, Typ}}, {AccWeight, {AccNodename, AccTyp}}) ->
              case {Typ =:= OfTyp andalso Weight < AccWeight} of
                {true} -> {Weight, {Nodename, Typ}};
                {false} -> {AccWeight, {AccNodename, AccTyp}}
              end

            end, FirstElem, FilterNodes)};
    false -> {nok,[]}
  end
.
markAs(EdgeManagerADT, Edge, Typ) ->
  {LastMarkedBranchWeight, ExtendedAdjacentNodes} = EdgeManagerADT,
  {Weight, {NodeName, _}} = Edge,
  werkzeug:pushSL(lists:filter(fun({ItemWeight, {ItemNodeName, _Typ}}) ->
    ItemNodeName =/= NodeName
  end, ExtendedAdjacentNodes), {Weight, {NodeName, Typ}})
.
convertToEdgeManagerADT(AdjacentNodes) ->
  ExtendedAdjacentNodes = lists:foldl(fun({Weight, Nodename}, Accu) -> werkzeug:pushSL(Accu, {Weight, {Nodename, basic}}) end, [], AdjacentNodes),
  {-1, ExtendedAdjacentNodes}
.



