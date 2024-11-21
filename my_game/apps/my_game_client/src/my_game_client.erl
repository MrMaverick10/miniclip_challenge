-module(my_game_client).
-export([start/0, interact_with_server/2,start/2,stop/1]).

start(_StartType, _StartArgs) ->
    io:format("My client is working!~n"),
    my_game_client_sup:start_link().

stop(_State) ->
    ok.

start() ->
    try
        {ok, Socket} = gen_tcp:connect('127.0.0.1', 4000, [binary, {packet, 0}, {active, false}]),
        {ok, PollingPid} = start_polling(Socket),
        io:format("Connected to the server!~n"),

        {ok, WelcomeMessage} = gen_tcp:recv(Socket, 0),
        io:format("[SERVER] : ~s~n", [WelcomeMessage]),

        Name = io:get_line("> "),
        gen_tcp:send(Socket, Name),

        {ok, Response} = gen_tcp:recv(Socket, 0),
        io:format("[SERVER] : ~s~n", [Response]),

        interact_with_server(Socket,PollingPid)
        
        catch
          error:Reason ->
              io:format("Error occurred while trying to connect: ~p~n", [Reason]);
          
          exit:_Timeout ->
              io:format("Connection attempt timed out after 5 seconds.~n")
        end.

start_polling(Socket) ->
    PollingPid = spawn(fun() -> poll(Socket) end),
    {ok, PollingPid}.

poll(Socket) ->
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
                    io:format("Connection closed by server.~n"),
                    exit(normal)
            end
    end.
  
interact_with_server(Socket,PollingPid) ->
  Command = io:get_line("> "),
  CommandStr = string:trim(Command),

  case CommandStr of
      "quit" ->
          gen_tcp:send(Socket,"quit\n"),
          gen_tcp:close(Socket),
          PollingPid ! stop,
          io:format("Exiting the game, see you soon...~n");
      _OtherCommand ->
          gen_tcp:send(Socket, CommandStr ++ "\n"),
          {ok, Response} = gen_tcp:recv(Socket, 0),
          io:format("[SERVER] : ~s~n", [Response]),
          
          interact_with_server(Socket,PollingPid)
  end.
