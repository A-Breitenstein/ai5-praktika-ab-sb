%%%-------------------------------------------------------------------
%%% @author abg628
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Okt 2013 11:37
%%%-------------------------------------------------------------------
-module(spammer).
-author("abg628").

%% API
-export([spam/1]).
spam(10) -> 0;
spam(N) -> {tipOfTheDaymessageServer,messageServer@PIGPEN} ! {self(),{getMsgId,node()}}, spam(N+1).
