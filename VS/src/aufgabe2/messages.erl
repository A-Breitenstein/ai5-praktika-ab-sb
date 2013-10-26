%%%-------------------------------------------------------------------
%%% @author abg628
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Okt 2013 11:18
%%%-------------------------------------------------------------------
-module(messages).
-author("abg628").

%% API
-export([sendConnect/3, sendInitiate/5, enqueue/2, sendReport/3]).

%% Enqueues the given MSG in NodeNames process messagequeue
enqueue(NodeName,MSG) ->
  Node = global:whereis_name(NodeName),
  Node ! MSG
.


sendInitiate(TargetNodeName,Level,FragmentName,State,Edge)->
 enqueue(TargetNodeName,{initiate,Level,FragmentName,State,Edge})
.

sendConnect(TargetNodeName,Level, Edge)->
  enqueue(TargetNodeName,{connect,Level,Edge})
.

sendReport(TargetNodeName,BestWeight,BestEdge) ->
  enqueue(TargetNodeName,{report,BestWeight,BestEdge})
.
sendAccept(TargetNodeName,Edge)->
  enqueue(TargetNodeName,{accept,Edge})
.