%% Copyright
-module(readerTest).
-author("Alex").

%% API
-export([test/0]).

square(X) ->io:format("~B~n",[X*X]).

test()->square(5).
