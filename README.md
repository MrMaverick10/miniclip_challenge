# My Game Project

## Description

"My Game" is a simple client-server application written in Erlang that demonstrates basic interaction using TCP connections. The project consists of two main components:

- **Server (`my_game_app`)**: Handles multiple client connections and manages user interactions such as room creation, deletion, and messaging.
- **Client (`my_game_client`)**: Connects to the server, allowing users to interact by creating public and private rooms, joining them, invite user to private rooms, sending public and private messages to other users.

## Changelog

### [v0.5.1]
#### Added
- The creator of a room can ban one or all the users of a room.
#### Changed
- Managed the reconnection from client when the server shutdown
- Managed the input validation for users and rooms creation

### [v0.5.0]
#### Added
- Users can create private rooms.
- Private rooms are visible only to the creator and the members.
- Users can invite other users to their private rooms.

### [v0.4.0]
#### Added
- Users can exchange private messages with one another.

### [v0.3.0]
#### Added
- Room management:
 - Users can create, destroy, list, join, and leave rooms.
- Users can send broadcast messages to all members of a room they belong to.

### [v0.2.0]
####Added
- Multi-user support:
  - Multiple users can connect to the server.
- Users can introduce themselves upon connection.

### [v0.1.0]
- First version, simple app with a welcome function and a math function

### Features
- **Multi-client support**: The server handles multiple simultaneous connections.
- **Room management**: Clients can create, list, delete, join, and leave chat rooms.
- **Private rooms**: Users can create private rooms and send invitations to other users.
- **Broadcast messaging**: Messages can be sent to all users in a room.
- **Private messaging**: Private messages can be sent between two users.

## Build, Compilation and Test Instructions

### Requirements
- **Windows**: Version 10 or higher.
- **Erlang/OTP**: Version 27 or higher.
- **Rebar3**: For project management in Erlang. If it is not in your system's `PATH`, you can execute it using the full path (e.g., `escript <path_to_rebar3>/rebar3` followed by `compile`, `eunit`, `release` and so on).

### Steps
1. Download or clone this repository

2. Navigate to the project's root directory. 
   ```
   cd <root-folder>/my_game/
   ```
   
3. Compile the projects (server and client): 
   ```
   rebar3 compile
   ```
  
   If successful, you should see a message like:
   ``` 
   ===> Verifying dependencies...
   ===> Analyzing applications...
   ===> Compiling my_game
   ===> Compiling my_game_client
   ```

4. To run the unit tests, execute the following command:
   ```
   cd <root-folder>/my_game/rebar3 eunit
   ```

   You should see something like this: 
   ```
   ===> Verifying dependencies...
   ===> Analyzing applications...
   ===> Compiling my_game
   ===> Compiling my_game_client
   ===> Performing EUnit tests...
   ..
   Finished in 0.048 seconds
   2 tests, 0 failures
   ```

## Release Instructions

### Steps

1. Generate the releases by running:
   ```
   rebar3 release --relname my_game_app and rebar3 release --relname my_game_client
   ```

   If successful, you should see a message like:
   ```
   ===> Verifying dependencies...
   ===> Analyzing applications...
   ===> Compiling my_game
   ===> Assembling release my_game-0.5.1... and ===> Assembling release my_game_client-0.5.1...
   ===> Release successfully assembled: _build/default/rel/my_game / _build/default/rel/my_game_client
   ```
 
   After the releases are generated, they will be located in:
   <root-folder>/my_game/_build/default/rel/my_game/ and <root-folder>/my_game/_build/default/rel/my_game_client/
  
2. To run the release in interactive mode (console) type these in two separate shells:
   ```
   cd  <root-folder>/my_game/_build/default/rel/my_game/bin
   .\my_game console -sname server_node -setcookie mygame
   
   cd  <root-folder>/my_game/_build/default/rel/my_game_client/bin
   .\my_game_client console -sname client_node -setcookie mygame
   ```

3. Once inside the Erlang server shell, start the tcp server using my_game_app:start() :
   ```
   1> my_game_app:start().
   ```
   
   You should see a message like: 
   
   Server listening on port 4000
   
4. Once inside the Erlang client shell, connect to server using my_game_client:start(), then enter your name:
   ```
   1> my_game_client:start().
   ```   

   You should see a message like: 
   ```   
   Connected to the server!
   [SERVER] : Welcome in this game! Please enter your name:
   >Tommaso                                                                                                                                                                                                 
   [SERVER] : Hello Tommaso! You are now connected to the game!
   AVAILABLE ACTIONS :
   - create_room [room name] to create a room
   - create_private_room [room name] to create a private room
   - invite_user [room name] [user name] to invite a user to a private room you created
   - list_rooms to see the available rooms
   -list_users to see all the other active users
   - join_room [room name] to join a room
   - leave_room [room name] to leave a room
   - delete_room [room name] to delete a room where you are the creator
   - send_message [room name] [message] to send a message to all the active users in the room
   - send_private_message [user name] [message] to send a private message to a specific user
   - ban_all [room name] to ban all the users from a room you created
   - ban_user [room name] [user name] to ban all the users from a room you created

   > join_room tommaso_room                                                                                                                                                                                     
   [SERVER] : You joined the room 'tommaso_room' successfully.

   > send_message tommaso_room Hello everybody! 

   [SERVER] : Message sent to all users in the room 'tommaso_room'.

   [BROADCAST MESSAGE IN tommaso_room FROM martina]: Hello Tommaso, i am Martina!

   > send_message tommaso_room Hello Martina, nice to meet you!  

   [SERVER] : Message sent to all users in the room 'tommaso_room'.
   ```

---

## Functions
### start/0 (my_game_app)
-This function start the server on a default port
Example:
```
my_game_app:start().
```
Result: 
```
Server listening on port 4000
```

### start/0 (my_game_client)
-This function start the connection between a client and the server, after the presentation from the client, the server list all the available actions
Example:
```
my_game_client:start(). 
```
Result: 
```
Connected to the server! Server says: Welcome in this game! Please enter your name:
```

---

##Persistent data with DynamoDB 

I attempted to integrate DynamoDB into my Erlang project but faced several challenges. Here's an overview of my attempts, the issues I encountered, and my progress.

I initially tried using the `erlcloud` library as a dependency:

```
{deps, [
    {erlcloud, "3.6.8"}
]}.


rebar3 get-deps
```

Unfortunately, I had multiple issues, including dependency resolution and errors related to SSL certificates. I tried downloading the repository locally and including it in the project, but the errors persisted.
To address the SSL issue, I included the certificate in the project:

```
{ssl, [{cacertfile, "C:/certs/cacert.pem"}]}.
```

However, this approach did not resolve the problem.

As the library approach failed, I decided to use Docker to set up a local instance of DynamoDB.



What i tried : 
- Installed Docker and AWS Command Line Interface
- Created a folder for the DynamoDB database
```mkdir C:\mini_clip_test\dynamodb```
-Created the container 
```docker run -d --name my_game_container -p 8001:8000 -v C:\mini_clip_test\dynamodb:/home/dynamodblocal/data amazon/dynamodb-local -jar DynamoDBLocal.jar -dbPath /home/dynamodblocal/data
```
-Launched the container 
```docker start my_game_container
```
-Created the tables 
```aws dynamodb create-table --table-name messages --attribute-definitions AttributeName=room_sender_receiver,AttributeType=S AttributeName=datetime,AttributeType=N --key-schema AttributeName=room_sender_receiver,KeyType=HASH AttributeName=datetime,KeyType=RANGE --billing-mode PAY_PER_REQUEST --endpoint-url http://localhost:8001
aws dynamodb create-table --table-name rooms --attribute-definitions AttributeName=name,AttributeType=S  --key-schema AttributeName=name,KeyType=HASH  --billing-mode PAY_PER_REQUEST --endpoint-url http://localhost:8001
aws dynamodb create-table --table-name users --attribute-definitions AttributeName=username,AttributeType=S --key-schema AttributeName=username,KeyType=HASH  --billing-mode PAY_PER_REQUEST --endpoint-url http://localhost:8001
```

Since the library was causing too many issues, I attempted to make direct HTTP requests to DynamoDB.

I wrote the following function to handle DynamoDB requests:

```
make_dynamodb_request(Action, Body) ->
    URL = "http://localhost:8001",
    Headers = [
        {"x-amz-target", Action},
        {"Authorization", "AWS4-HMAC-SHA256 Credential=test/20231124/test/dynamodb/aws4_request, SignedHeaders=host;x-amz-date, Signature=test"}
    ],

    Response = httpc:request(post, {URL, Headers, "application/x-amz-json-1.0", Body}, [], []),
    io:format("Response: ~p~n", [Response]),
    case Response of
        {ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, ResponseBody}} ->
            io:format("HTTP Success: ~p~nHeaders: ~p~nBody: ~s~n", [200, ResponseHeaders, ResponseBody]),
            {ok, ResponseBody};
        {ok, {{"HTTP/1.1", StatusCode, StatusText}, ResponseHeaders, ResponseBody}} ->
            io:format("HTTP Error: Status ~p ~p~nHeaders: ~p~nBody: ~s~n", [StatusCode, StatusText, ResponseHeaders, ResponseBody]),
            {error, {StatusCode, ResponseBody}};
        {error, Reason} ->
            io:format("HTTP Failure: Reason ~p~n", [Reason]),
            {error, {500, Reason}}
    end.
```

Here’s an example of how I tried to create a room in the rooms table:

```
create_room(Socket, RoomName, ClientName) ->
    Body = iolist_to_binary(
        io_lib:format(
            "{\"TableName\": \"~s\", \"Item\": {\"name\": {\"S\": \"~s\"}, \"creator\": {\"S\": \"~s\"}, \"private\": {\"BOOL\": false}}}",
            ["rooms", RoomName, ClientName]
        )
    ),
    io:format("Generated JSON for PutItem: ~s~n", [Body]),

    Request = make_dynamodb_request("DynamoDB_20120810.PutItem",Body),

    case Request  of
        {ok, "{}"} ->
            gen_tcp:send(Socket, "Room created successfully!\n");
        {ok, {{200, _StatusText}, _Headers, "{}"}} ->
            gen_tcp:send(Socket, "Room created successfully, response was empty!\n");
        {ok, {{400, _StatusText}, _Headers, ResponseBody}} ->
            gen_tcp:send(Socket, io_lib:format("Failed to create room. Error: ~s\n", [ResponseBody]));
        {error, Reason} ->
            gen_tcp:send(Socket, io_lib:format("Unexpected error: ~p\n", [Reason]))
    end.
```

The combination of using a passive socket, client-side polling, and simultaneous HTTP requests to DynamoDB likely caused a race condition or resource contention, leading to intermittent failures in either the HTTP calls to DynamoDB or the socket operations.

Having no prior experience with Erlang, Docker, or DynamoDB, I faced several challenges along the way. Nonetheless, I explored multiple approaches to integrate DynamoDB into my Erlang application, including using erlcloud, Docker, AWS CLI, and direct HTTP requests. Although I couldn't fully implement the desired functionality, these efforts demonstrate my determination and problem-solving commitment.

Thank you for your interest in my application.