%% Copyright
-module(server).
-author("Alex").

%% API
-export([hallo/0]).
-import(reader,[test/0]).

hallo() ->
  test().