-module(my_game_client).
-export([start/0, interact_with_server/2, start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case ets:info(client_state) of
        undefined ->
            ets:new(client_state, [named_table, public, {keypos, 1}]);
        _ -> ok
    end,
    start_client_connection().

stop(_State) ->
    ok.

get_my_username() ->
    case ets:lookup(client_state, user_name) of
        [{user_name, Name}] -> "[" ++ Name ++ "] > ";
        [] -> "[ME] > "
    end.

start() ->
      start(normal,[]).

start_client_connection() -> 
    case gen_tcp:connect('127.0.0.1', 4000, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            {ok, PollingPid} = start_polling(Socket),
            io:format("Connected to the server!~n"),

            case gen_tcp:recv(Socket, 0) of
                {ok, WelcomeMessage} ->
                    io:format("[SERVER] : ~s~n", [WelcomeMessage]),

                    Name = io:get_line(get_my_username()),
                    gen_tcp:send(Socket, Name),
                    ets:insert(client_state, {user_name, string:trim(Name)}),

                    case gen_tcp:recv(Socket, 0) of
                        {ok, Response} ->
                            io:format("[SERVER] : ~s~n", [Response]),
                            interact_with_server(Socket, PollingPid);
                        {error, closed} ->
                            io:format("Connection closed by server during login. Restarting...\n"),
                            start()
                    end;
                {error, closed} ->
                    io:format("Connection closed by server during handshake. Restarting...\n"),
                    start()
            end;
        {error, Reason} ->
            io:format("Failed to connect to the server: ~p. Retrying...\n", [Reason]),
            timer:sleep(3000),
            start()
    end.

start_polling(Socket) ->
    PollingPid = spawn(fun() -> poll(Socket) end),
    {ok, PollingPid}.

wait_for_reconnect() ->
    Command = io:get_line(get_my_username()),
    case string:trim(Command) of
        "reconnect" ->
            io:format("Attempting to reconnect...\n"),
            start();
        _ ->
            io:format("Invalid command. Type 'reconnect' to restart.\n"),
            wait_for_reconnect()
    end.

poll(Socket) ->
    case ets:info(client_state) of
        undefined ->
            io:format("Waiting for client state table initialization.\n"),
            timer:sleep(1000),
            poll(Socket);
        _ ->
            case ets:lookup(client_state, user_name) of
                [{user_name, _}] ->
                    receive
                        stop ->
                            ok
                        after 1000 ->
                            case gen_tcp:recv(Socket, 0, 0) of
                                {ok, Data} ->
                                    io:format("~s", [binary:bin_to_list(Data)]),
                                    poll(Socket);
                                {error, timeout} ->
                                    poll(Socket);
                                {error, closed} ->
                                    io:format("Connection closed by server. Type 'reconnect' to restart.\n"),
                                    wait_for_reconnect()
                            end
                    end;
                [] ->
                    poll(Socket)
            end
    end.

interact_with_server(Socket, PollingPid) ->
    try
        Command = io:get_line(get_my_username()),
        CommandStr = string:trim(Command),

        case CommandStr of
            "quit" ->
                gen_tcp:send(Socket, "quit\n"),
                PollingPid ! stop, 
                io:format("Exiting the game, see you soon...~n"),
                gen_tcp:close(Socket); 
            "reconnect" ->
                PollingPid ! stop,
                io:format("Attempting to reconnect...\n"),
                gen_tcp:close(Socket),
                start(); 
            _OtherCommand ->
                gen_tcp:send(Socket, CommandStr ++ "\n"),
                case gen_tcp:recv(Socket, 0) of
                    {ok, Response} ->
                        % Mostra la risposta dal server senza includere il nome del client
                        io:format("[SERVER] : ~s~n", [Response]),
                        interact_with_server(Socket, PollingPid);
                    {error, closed} ->
                        io:format("Server closed the connection.~n"),
                        PollingPid ! stop, 
                        gen_tcp:close(Socket),
                        exit(normal)
                end
        end
    catch
        error:Reason ->
            io:format("Error during interaction: ~p~n", [Reason]),
            PollingPid ! stop,
            gen_tcp:close(Socket),
            exit(Reason);
        exit:Reason ->
            io:format("Interaction terminated unexpectedly: ~p~n", [Reason]),
            PollingPid ! stop,
            gen_tcp:close(Socket),
            exit(Reason)
    end.

