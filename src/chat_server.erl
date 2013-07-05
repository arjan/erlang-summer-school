%% A simple chat server.

-module(chat_server).

-export([start/0, loop/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(server, Pid).


loop(Clients) ->
    receive

        {connect, ClientPid} ->
            io:format("A new client connects! ~p~n", [ClientPid]),
            %% Add the ClientPid to the list of clients.
            chat_server:loop( [ ClientPid|Clients] );

        {message, MessageString} ->
            send_message_to_clients(Clients, MessageString),
            chat_server:loop(Clients);

        SomeMessage ->
            io:format("Received something unknown: ~p~n", [SomeMessage]),
            chat_server:loop(Clients)
    end.


%% @doc Send the message to each client.
send_message_to_clients([], _MessageString) ->
    %% nobody left to send to
    ok;
send_message_to_clients([Client | OtherClients], MessageString) ->
    %% Send to the first client in the client list.
    Client ! MessageString,
    %% Send to the other clients.
    send_message_to_clients(OtherClients, MessageString).

