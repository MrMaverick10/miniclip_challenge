# My Game Project

## Description

"My Game" is a simple client-server application written in Erlang that demonstrates basic interaction using TCP connections. The project consists of two main components:

- **Server (`my_game_app`)**: Handles multiple client connections and manages user interactions such as room creation, deletion, and messaging.
- **Client (`my_game_client`)**: Connects to the server, allowing users to interact by creating rooms, joining them, and sending messages to other participants.

### Features
- **Multi-client support**: The server handles multiple simultaneous connections.
- **Room management**: Clients can create, list, delete, join, and leave chat rooms.
- **Broadcast messaging**: Messages can be sent to all users in a room.
- **Graceful disconnection**: Clients can quit cleanly, ensuring server resources are released.

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

3. Once inside the Erlang server shell, start the tcp server using my_game_app:start() :

   1> my_game_app:start().
   
   You should see a message like: 
   
   Server listening on port 4000
   
4. Once inside the Erlang client shell, connect to server using my_game_client:start(), then enter your name:

   1> my_game_client:start().
   
   You should see a message like: 
   
   Connected to the server!
   [SERVER] : Welcome in this game! Please enter your name:
   >Tommaso                                                                                                                                                                                                 
   [SERVER] : Hello Tommaso! You are now connected to the game!
   AVAILABLE ACTIONS :
   - create_room [room name] to create a room
   - list_rooms to see the available rooms
   - join_room [room name] to join a room
   - delete_room [room name] to delete a room where you are the creator
   - send_message [room name] [message] to send a message to all the active users in the room

   > join_room tommaso_room                                                                                                                                                                                     
   [SERVER] : You joined the room 'tommaso_room' successfully.

   > send_message tommaso_room Hello everybody! 

   [SERVER] : Message sent to all users in the room 'tommaso_room'.

   [BROADCAST MESSAGE IN ROOM [tommaso_room] FROM [martina]]: Hello Tommaso, i am Martina!

   > send_message tommaso_room Hello Martina, nice to meet you!  

   [SERVER] : Message sent to all users in the room 'tommaso_room'.

---

## Functions
### start/0 (my_game_app)
-This function start the server on a default port
Example:
my_game_app:start().
Result: 
Server listening on port 4000

### start/0 (my_game_client)
-This function start the connection between a client and the server, after the presentation from the client, the server list all the available actions
Example:
my_game_client:start(). 
Result: 
Connected to the server! Server says: Welcome in this game! Please enter your name:
