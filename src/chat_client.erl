%% A simple chat client.

-module(chat_client).

-export([connect/1, send/2, clientloop/1]).


connect(Server) ->
    spawn(fun() ->
                  do_connect(Server) end).


do_connect(Server) ->
    %% Send "connect" msg and wait for return
    Server ! {connect, self()},
    clientloop(Server).


clientloop(Server) ->
    receive
        {send_to_server, Msg} ->
            Server ! {message, Msg},
            chat_client:clientloop(Server);

        Something ->
            io:format("Received something from the server"),
            io:format("--> ~p~n", [Something]),
            chat_client:clientloop(Server)
    end.


%% send a message from the erlang shell to the client process, which
%% will send it to the server process.
send(Client, Message) ->
    Client ! {send_to_server, Message}.
