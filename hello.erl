%% Comment
-module(hello).
-export([hello/0,
         world/0
        ]).

%% @doc This is the documentation
hello() ->
    io:format("Hello, world!~n").

world() ->
    aap.


