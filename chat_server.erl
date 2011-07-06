%% A simple chat server.

-module(chat_server).

-export([start/1]).

start(Name) ->
    Pid = spawn(fun() -> loop([]) end),
    register(Name, Pid).

loop(Clients) ->
    receive 
        _Any ->
            io:format("Received: ~p~n", [_Any]),
            loop(Clients)
    end.

