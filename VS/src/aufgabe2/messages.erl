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
-export([sendConnect/3, sendInitiate/5, enqueue/2, sendReport/3, sendAccept/2,
         sendTest/4, sendChangeRoot/2, sendReject/2]).

%% Enqueues the given MSG in NodeNames process messagequeue
enqueue(NodeName,MSG) ->
  Node = global:whereis_name(NodeName),
%%   io:format ("Trying to send ~p message to Node ~p ( ~p )~n", [MSG,NodeName,Node]),
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

sendTest(TargetNodeName,Level,Fragment,Edge)->
  enqueue(TargetNodeName,{test,Level,Fragment,Edge})
.

sendChangeRoot(TargetNodeName,Edge)->
  enqueue(TargetNodeName,{changeroot,Edge})
.
sendReject(TargetNodeName,Edge)->
  enqueue(TargetNodeName,{reject,Edge})
.