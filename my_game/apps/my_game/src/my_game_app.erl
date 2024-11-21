-module(my_game_app).
-behaviour(application).
-export([start/2, start/0, stop/1,
         start_tcp_server/1,accept_connections/1,handle_client/1,
         receive_name/1,handle_game_interaction/2,receive_game_action/2]).
-define(ACTION_SELECTION,"\nAVAILABLE ACTIONS : "
                         "\n- create_room [room name] to create a room"
                         "\n- list_rooms to see the available rooms"
                         "\n- join_room [room name] to join a room"
                         "\n- leave_room [room name] to leave a room"
                         "\n- delete_room [room name] to delete a room where you are the creator"
                         "\n- send_message [room name] [message] to send a message to all the users in the room\n").

-record(room, {name, users = [], creator}).

start(_StartType, _StartArgs) ->
    io:format("My game is working!~n"),
    ets:new(rooms_table, [named_table, public, {keypos, 1}]),
    ets:new(active_users, [named_table, public, {keypos, 1}]),
    my_game_sup:start_link(),
    start_tcp_server(4000).

start() -> 
  start(normal,[]).

stop(_State) ->
    ets:delete(rooms_table),
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
  TrimmedName = string:trim(ClientName),
  io:format("Client's name: ~s~n", [TrimmedName]),
  ets:insert(active_users, {TrimmedName, Socket}),
  handle_game_interaction(Socket,TrimmedName).
  
handle_game_interaction(Socket,ClientName) -> 
  gen_tcp:send(Socket, "Hello " ++ string:trim(ClientName) ++ "! You are now connected to the game!" ++ ?ACTION_SELECTION),
  receive_game_action(Socket,ClientName).
  
receive_game_action(Socket, ClientName) -> 
    {ok, Action} = gen_tcp:recv(Socket, 0),
    ActionStr = string:trim(binary:bin_to_list(Action)),
    io:format("Received action ~s from client ~s~n", [ActionStr, ClientName]),

    case string:tokens(ActionStr, " ") of 
        ["create_room", RoomName] ->
            create_room(Socket, RoomName, ClientName),
            receive_game_action(Socket, ClientName);
        
        ["list_rooms"] ->
            list_rooms(Socket),
            receive_game_action(Socket, ClientName);

        ["join_room", RoomName] ->
          join_room(Socket, RoomName, ClientName),
          receive_game_action(Socket, ClientName);

        ["leave_room", RoomName] ->
            leave_room(Socket, RoomName, ClientName),
            receive_game_action(Socket, ClientName);

        ["delete_room", RoomName] ->
            delete_room(Socket,RoomName,ClientName),
            receive_game_action(Socket,ClientName);

        ["list_users"] ->
            list_users(Socket, ClientName),
            receive_game_action(Socket, ClientName);

        ["send_message", RoomName | Rest] ->
          Message = string:join(Rest, " "),
          send_message(Socket, RoomName, ClientName, Message),
          receive_game_action(Socket, ClientName);

        ["quit"] -> 
            gen_tcp:send(Socket, "Goodbye!\n"),
            gen_tcp:close(Socket),
            ok;

        _OtherCommand ->
            gen_tcp:send(Socket, "Unknown command. " ++ ?ACTION_SELECTION),
            receive_game_action(Socket, ClientName)
    end.

create_room(Socket, RoomName, ClientName) ->
    RoomKey = list_to_binary(RoomName),
    TrimmedName = string:trim(ClientName),
    case ets:lookup(rooms_table, RoomKey) of
        [] ->
            Room = #room{name = RoomName, users = [TrimmedName], creator = TrimmedName},
            ets:insert(rooms_table, {RoomKey, Room}),
            gen_tcp:send(Socket, "Room created successfully!\n");
        _ ->
            gen_tcp:send(Socket, "Room already exists.\n")
    end.

list_rooms(Socket) ->
    Rooms = ets:tab2list(rooms_table),
    case Rooms of
        [] ->
            gen_tcp:send(Socket, "No available rooms.\n");
        _ ->
            RoomDescriptions = [
                describe_room(Room) || {_RoomName, Room} <- Rooms
            ],
            gen_tcp:send(Socket, "Available rooms:\n" ++ string:join(RoomDescriptions, "\n\n") ++ "\n")
    end.

list_users(Socket, ClientName) ->
    AllUsers = ets:tab2list(active_users),  
    OtherUsers = [User || {User, _Socket} <- AllUsers, User =/= ClientName],  
    case OtherUsers of
        [] ->
            gen_tcp:send(Socket, "No other active users.\n");
        _ ->
            gen_tcp:send(Socket, "Active users:\n" ++ string:join(OtherUsers, "\n") ++ "\n")
    end.

describe_room(#room{name = Name, users = Users, creator = Creator}) ->
    SortedUsers = lists:sort(Users),  
    io_lib:format("Room Name: ~s\nCreator: ~s\nUsers: ~s", 
                  [Name, Creator, string:join(SortedUsers, ", ")]).

join_room(Socket, RoomName, ClientName) ->
    RoomKey = list_to_binary(RoomName),
    TrimmedName = string:trim(ClientName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, users = Users, creator = _} = Room}] ->
            case lists:member(TrimmedName, Users) of
                true ->
                    gen_tcp:send(Socket, "You are already in the room '" ++ Name ++ "'.\n");
                false ->
                    UpdatedRoom = Room#room{users = [TrimmedName | Users]},
                    ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
                    gen_tcp:send(Socket, "You joined the room '" ++ Name ++ "' successfully.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

leave_room(Socket, RoomName, ClientName) ->
    RoomKey = list_to_binary(RoomName),
    TrimmedName = string:trim(ClientName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, users = Users, creator = _} = Room}] ->
            case lists:member(TrimmedName, Users) of
                true ->
                    UpdatedUsers = lists:delete(TrimmedName, Users),
                    UpdatedRoom = Room#room{users = UpdatedUsers},
                    ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
                    gen_tcp:send(Socket, "You left the room '" ++ Name ++ "' successfully.\n");
                false ->
                    gen_tcp:send(Socket, "You are not a member of the room '" ++ Name ++ "'.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

delete_room(Socket, RoomName, ClientName) -> 
  RoomKey = list_to_binary(RoomName),
    TrimmedName = string:trim(ClientName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, users = _, creator = Creator} = _}] ->
            case TrimmedName of
                Creator ->
                    ets:delete(rooms_table, RoomKey),
                    gen_tcp:send(Socket, "Room '" ++ Name ++ "' has been deleted successfully.\n");
                _ ->
                    gen_tcp:send(Socket, "You are not the creator of the room '" ++ Name ++ "'. Only the creator can delete it.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

send_message(Socket, RoomName, ClientName, Message) ->
    RoomKey = list_to_binary(RoomName),
    TrimmedName = string:trim(ClientName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, users = Users, creator = _}}] ->
            case lists:member(TrimmedName, Users) of
                true ->
                    OtherUsers = lists:delete(TrimmedName, Users),  % Exclude sender
                    gen_tcp:send(Socket, "Message sent to all users in the room '" ++ Name ++ "'.\n"),
                    broadcast_message(OtherUsers, Name, TrimmedName, Message);
                false ->
                    gen_tcp:send(Socket, "You are not a member of the room '" ++ Name ++ "'. Join the room to send a message.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

broadcast_message([], _RoomName, _Sender, _Message) ->
    ok; 
broadcast_message([User | Rest], RoomName, Sender, Message) ->
    case ets:lookup(active_users, User) of
        [{_User, UserSocket}] ->
            FormattedMessage = io_lib:format("[BROADCAST MESSAGE IN ~s FROM ~s]: ~s\n", [RoomName, Sender, Message]),
            gen_tcp:send(UserSocket, lists:flatten(FormattedMessage));
        [] ->
            io:format("User ~s is not actively connected.\n", [User])
    end,
    broadcast_message(Rest, RoomName, Sender, Message).
