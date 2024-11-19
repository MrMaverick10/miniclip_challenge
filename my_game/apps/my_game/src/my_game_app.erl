-module(my_game_app).
-behaviour(application).
-export([start/2, stop/1,who_are_you/0,do_some_math/5,
         start_tcp_server/1,accept_connections/1,handle_client/1,
         receive_name/1,handle_game_interaction/2,receive_game_action/2]).

start(_StartType, _StartArgs) ->
    io:format("My game is working!~n"),
    my_game_sup:start_link().

stop(_State) ->
    ok.
    
start_tcp_server(Port) -> 
  {ok,ListenSocket} = gen_tcp:listen(Port,[binary,{packet,0},{active,false}]),
  io:format("Server listening on port ~p~n", [Port]),
  accept_connections(ListenSocket).
  
accept_connections(ListenSocket) -> 
  {ok,Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> handle_client(Socket) end),
  accept_connections(ListenSocket).

handle_client(Socket) ->
  io:format("New client connected!~n"),
  gen_tcp:send(Socket,"Welcome in this game! Please enter your name: "),
  receive_name(Socket).
  
receive_name(Socket) ->
  {ok, Data} = gen_tcp:recv(Socket,0),
  ClientName = binary:bin_to_list(Data),
  io:format("Client's name: ~s~n", [ClientName]),
  handle_game_interaction(Socket,ClientName).
  
handle_game_interaction(Socket,ClientName) -> 
  gen_tcp:send(Socket, "Hello " ++ string:trim(ClientName) ++ "! You are now connected to the game!"),
  receive_game_action(Socket,ClientName).
  
receive_game_action(Socket,ClientName) -> 
  {ok, Action} = gen_tcp:recv(Socket,0),
  ActionStr = string:trim(binary:bin_to_list(Action)),
  io:format("Received action ~s from client ~s~n", [ActionStr,ClientName]),
  
  case ActionStr of 
    "quit" -> 
      %%gen_tcp:send(Socket,"Hello " ++ ClientName ++ ", it has been a pleasure to meet you!~n"),
      gen_tcp:close(Socket),
      ok;
    _OtherCommand ->
      gen_tcp:send(Socket,"Action processed. Type 'quit' to exit the game: "),
      receive_game_action(Socket,ClientName)
  end.
  
%%test functions
who_are_you() ->
    "Hello, i am a new game!".

do_some_math(A, B, C, D, E) -> 
    (((A + B) - C) * D) / E.
