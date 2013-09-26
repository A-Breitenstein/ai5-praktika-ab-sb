%% Copyright
-module(writer).
-author("Alex").

%% API
-export([say/0]).

another(Str) ->
  io:format(Str).

say() ->
  Str="Hallo",
  another(Str).


