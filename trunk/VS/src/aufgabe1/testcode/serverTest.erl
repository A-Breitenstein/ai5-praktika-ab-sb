%% Copyright
-module(serverTest).
-author("Alex").

%% API
-export([hallo/0]).
-import(readerTest,[test/0]).

hallo() ->
  test().