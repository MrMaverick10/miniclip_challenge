# My Game Project

## Description

"My Game" is an Erlang application that demonstrates the basic client-server interaction with TCP connections. The project includes two main components:

- **Server (`my_game_app`)**: A multi-client server that can accept multiple concurrent connections, manage game interactions, and respond to client commands.
  
- **Client (`my_game_client`)**: A client application to connect to the server, send user name, and receive responses.

### Features:
- The server accepts multiple simultaneous connections from different clients.
- The client sends its name to the server and receives a personalized welcome message.
- The client can send commands such as 'quit' to exit the game.

## Build, Compilation and Test Instructions

### Requirements
- **Erlang/OTP**: Version 27 or higher.
- **Rebar3**: For project management in Erlang. If it is not in your system's `PATH`, you can execute it using the full path (e.g., `escript <path_to_rebar3>/rebar3` followed by `compile`, `eunit`, `release` and so on).

### Steps
1. Download or clone this repository

2. Navigate to the project's root directory. 
   cd <root-folder>/my_game/
   
3. Compile the projects (server and client): 
   rebar3 compile
  
   If successful, you should see a message like:
   
  ===> Verifying dependencies...
  ===> Analyzing applications...
  ===> Compiling my_game
  ===> Compiling my_game_client

4. To run the unit tests, execute the following command:
   cd <root-folder>/my_game/
   rebar3 eunit

   You should see something like this: 

   ===> Verifying dependencies...
   ===> Analyzing applications...
   ===> Compiling my_game
   ===> Compiling my_game_client
   ===> Performing EUnit tests...
   ..
   Finished in 0.048 seconds
   2 tests, 0 failures

## Release Instructions

### Steps

1. Generate the releases by running:

   rebar3 release --relname my_game_app and rebar3 release --relname my_game_client

   If successful, you should see a message like:

  ===> Verifying dependencies...
  ===> Analyzing applications...
  ===> Compiling my_game
  ===> Assembling release my_game-0.1.0... and ===> Assembling release my_game_client-0.1.0...
  ===> Release successfully assembled: _build/default/rel/my_game / _build/default/rel/my_game_client

  After the releases are generated, they will be located in:
  <root-folder>/my_game/_build/default/rel/my_game/ and <root-folder>/my_game/_build/default/rel/my_game_client/
  
2. To run the release in interactive mode (console) type these in two separate shells:

   cd  <root-folder>/my_game/_build/default/rel/my_game/bin
   .\my_game console -sname server_node -setcookie mygame
   
   cd  <root-folder>/my_game/_build/default/rel/my_game_client/bin
   .\my_game_client console -sname client_node -setcookie mygame

3. Once inside the Erlang server shell, start the tcp server using my_game_app:start_tcp_server([PORT])

   1> my_game_app:start_tcp_server(4000).
   
   You should see a message like: 
   
   Server listening on port 5000
   
4. Once inside the Erlang client shell, connect to server using my_game_client:start(), then enter your name and then type quit to close the connection:

   1> my_game_client:start().
   
   You should see a message like: 
   
   2> my_game_client:start().
   Connected to the server!
   Server says: Welcome in this game! Please enter your name:
   > Tommaso
   Server says: Hello Tommaso! You are now connected to the game!
   There's nothing left to do here, please type 'quit' to exit : > quit
   Exiting the game, see you soon...
   ok

---

## Functions
### start_tcp_server/1
-This function start the server on a given port as parameter
Example:
my_game_app:start_tcp_server(4000).
Result: 
Server listening on port 5000

### start/0
-This function start the connection between a client and the server, and then a conversation will start between them
Example:
my_game_client:start(). 
Result: 
Connected to the server! Server says: Welcome in this game! Please enter your name:
