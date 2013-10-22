%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Okt 2013 18:30
%%%-------------------------------------------------------------------
-module(ggTNodeManager).
-author("Sven").

%% API
-export([createNodeCircle/1, pickRandomGGTNode/1, informGGTNodes/2, chooseSomeNodes/1, sendY/1, killAllGGTNodes/1, registerGGTNode/2, promptGGTNodes/1, nudgeGGTNodes/1]).

%% 5. +createNodeCircle(GGTNodeList): GGTNodeList
createNodeCircle(GGTNodeList) -> 0.
%% 5. +pickRandomGGTNode(GGTNodeList) : GGTNode
pickRandomGGTNode(GGTNodeList) -> 0.
%% 5. +registerGGTNode(GGTNodeList) : GGTNodeList
registerGGTNode(GGTNodeList,GGTNode) -> 0.
%% 6. +informGGTNodes(GGTNodeList,Startwert) : void
informGGTNodes(GGTNodeList,Startwert) ->0.
%% 7. +chooseSomeNodes(GGTNodeList) : GGTNodeList
chooseSomeNodes(GGTNodeList) -> 0.
%% 9. +sendY(GGTNode) : void
sendY(GGTNode) -> 0.
%% 11. +killAllGGTNodes(GGTNodeList)  :  bool
killAllGGTNodes(GGTNodeList) -> 0.

%% von Klaucks Schnittstellen Beschreibung
%% des Koordinatorsabgeleitete Funktionen
%% +promptGGTNodes(GGTNodeList) : Liste<{Mi,GGTNode}>
promptGGTNodes(GGTNodeList)-> 0.
%% +nudgeGGTNodes(GGTNodeList) : Liste<LifeState>
nudgeGGTNodes(GGTNodeList) -> 0.