%% A simple chat server.

-module(chat_client).

-export([connect/2, send/2]).

-record(client, {pid, host}).

connect(Name, Node) ->
    ClientPid = spawn(fun() -> loop() end),
    {Name, Node} ! {connect, ClientPid},
    #client{pid=ClientPid, host={Name, Node}}.

loop() ->
    receive 
        _Any ->
            io:format("Received: ~p~n", [_Any]),
            loop()
    end.


send(#client{pid=Pid, host=Host}, Message) ->
    Host ! {message, {Pid, Message}}.

