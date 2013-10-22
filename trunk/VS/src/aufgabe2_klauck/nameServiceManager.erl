%%%-------------------------------------------------------------------
%%% @author Sven
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Okt 2013 22:42
%%%-------------------------------------------------------------------
-module(nameServiceManager).
-author("Sven").

%% API
-export([getNameService/1, bind/2, unbind/2, lookup/1]).

%% Nachdem ein ping auf den Knoten des Namensdienstes gemacht wurde (net_adm:ping(NameserviceNode)),
%% erhält man die Adresse durch:
%% Nameservice = global:whereis_name(nameservice).
%% Rebinden eines Dienstes (erstmaliges oder wiederholtes binden):
%% Request
%% Nameservice ! {self(),{rebind,meindienst,node()}},
%% Response
%% {nameservice,ok}
%%
%% Lookup für einen Dienst:
%% Request
%% Nameservice ! {self(),{lookup,meindienst}},
%% Responses
%% {nameservice,{pin,{Name,Node}}}
%% {nameservice,not_found}
%%
%%
%% Unbind eines Dienstes:
%% Request
%% Nameservice ! {self(),{unbind,meindienst}},
%% Response
%% {nameservice,ok}

getNameService(NameserviceNode)->
  net_adm:ping(NameserviceNode),
  global:whereis_name(nameservice)
.
bind(ProcessName,Name) -> 0.
unbind(ProcessName,Name) ->0.
lookup(Name) -> 0.


logging(Datei,Inhalt) -> Known = erlang:whereis(koordinatorNAME),
  case Known of
    undefined -> PIDlogklc = spawn(fun() -> logloop(0) end),
      erlang:register(logklc,PIDlogklc);
    _NotUndef -> ok
  end,
  logklc ! {Datei,Inhalt},
  ok.

logstop( ) -> 	Known = erlang:whereis(logklc),
  case Known of
    undefined -> false;
    _NotUndef -> logklc ! kill, true
  end.

logloop(Y) -> 	receive
                 {Datei,Inhalt} -> io:format(Inhalt),
                   file:write_file(Datei,Inhalt,[append]),
                   logloop(Y+1);
                 kill -> true
               end.