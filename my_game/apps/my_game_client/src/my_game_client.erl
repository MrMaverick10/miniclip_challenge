-module(my_game_client).
-export([start/0, interact_with_server/1,start/2,stop/1]).

start(_StartType, _StartArgs) ->
    io:format("My client is working!~n"),
    my_game_client_sup:start_link().

stop(_State) ->
    ok.

start() ->
    try
        {ok, Socket} = gen_tcp:connect('127.0.0.1', 4000, [binary, {packet, 0}, {active, false}]),
        io:format("Connected to the server!~n"),

        {ok, WelcomeMessage} = gen_tcp:recv(Socket, 0),
        io:format("Server says: ~s~n", [WelcomeMessage]),

        Name = io:get_line("> "),
        gen_tcp:send(Socket, Name),

        {ok, Response} = gen_tcp:recv(Socket, 0),
        io:format("Server says: ~s~n", [Response]),

        interact_with_server(Socket)
        catch
          error:Reason ->
              io:format("Error occurred while trying to connect: ~p~n", [Reason]);
          
          exit:Timeout ->
              io:format("Connection attempt timed out after 5 seconds.~n")
        end.
  
interact_with_server(Socket) ->
  io:format("There's nothing left to do here, please type 'quit' to exit : "),
  Command = io:get_line("> "),
  CommandStr = string:trim(Command),

  case CommandStr of
      "quit" ->
          gen_tcp:send(Socket,"quit\n"),
          gen_tcp:close(Socket),
          io:format("Exiting the game, see you soon...~n");
      _OtherCommand ->
          gen_tcp:send(Socket, CommandStr ++ "\n"),
          {ok, Response} = gen_tcp:recv(Socket, 0),
          io:format("Server says: ~s~n", [Response]),
          
          interact_with_server(Socket)
  end.
