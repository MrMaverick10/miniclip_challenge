-module(my_game_app).
-behaviour(application).
-export([start/2, start/0, stop/1, start_tcp_server/1]).
-define(DEFAULT_PORT,4000).
-define(ADMIN_USERNAME,"admin").
-define(ACTION_SELECTION,"\nAVAILABLE ACTIONS : "
                         "\n- create_room [room name] to create a room"
                         "\n- create_private_room [room name] to create a private room"
                         "\n- invite_user [room name] [user name] to invite a user to a private room you created"
                         "\n- list_rooms to see the available rooms"
                         "\n -list_users to see all the other active users"
                         "\n- join_room [room name] to join a room"
                         "\n- leave_room [room name] to leave a room"
                         "\n- delete_room [room name] to delete a room where you are the creator"
                         "\n- send_private_message [user name] [message] to send a private message to a specific user"
                         "\n- send_message [room name] [message] to send a message to all the users in the room"
                         "\n- ban_all [room name] to ban all the users from a room you created."
                         "\n- ban_user [room name] [user name] to ban all the users from a room you created.\n").

-record(room, {name, users = [], creator, private = false, invited_users = [], banned_users = []}).

start(_StartType, _StartArgs) ->
    io:format("My game is working!~n"),
    ets:new(rooms_table, [named_table, public, {keypos, 1}]),
    ets:new(active_users, [named_table, public, {keypos, 1}]),
    my_game_sup:start_link(),
    start_tcp_server(?DEFAULT_PORT).

start() -> 
  start(normal,[]).

stop(_State) ->
    ets:delete(active_users),
    ets:delete(rooms_table),
    ok.
    
start_tcp_server(Port) -> 
  {ok,ListenSocket} = gen_tcp:listen(Port,[binary,{packet,0},{active,false}]),
  io:format("Server listening on port ~p~n", [Port]),
  accept_connections(ListenSocket).
  
accept_connections(ListenSocket) -> 
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_connections(ListenSocket);
        {error, Reason} ->
            io:format("Error accepting connection: ~p~n", [Reason])
    end.

handle_client(Socket) ->
  io:format("New client connected!~n"),
  gen_tcp:send(Socket,"Welcome in this game! Please enter your name: "),
  receive_name(Socket).
  
receive_name(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ClientName = string:trim(binary:bin_to_list(Data)),
            case validate_name(ClientName) of
                ok ->
                    case ets:lookup(active_users, ClientName) of
                        [] ->
                            ets:insert(active_users, {ClientName, Socket}),
                            io:format("Client name registered: ~s~n", [ClientName]),
                            gen_tcp:send(Socket, "Hello " ++ ClientName ++ "! You are now connected to the game.\n"),
                            handle_game_interaction(Socket, ClientName);
                        _ ->
                            gen_tcp:send(Socket, "Name is already in use. Please enter another name: "),
                            receive_name(Socket)
                    end;
                {error, Reason} ->
                    gen_tcp:send(Socket, "Invalid name: " ++ Reason ++ "\nPlease enter another name: "),
                    receive_name(Socket)
            end;
        {error, closed} ->
            io:format("Client disconnected before entering a name.~n");
        {error, Reason} ->
            io:format("Error receiving name: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

validate_name(Name) ->
    TrimmedName = string:trim(Name),
    case true of
        _ when TrimmedName =:= "" ->
            {error, "Name cannot be empty"};
        _ when length(TrimmedName) > 20 ->
            {error, "Name must not exceed " ++ integer_to_list(20) ++ " characters"};
        _ ->
            case has_space(TrimmedName) of
                true ->
                    {error, "Name must not contain spaces"};
                false ->
                    case is_alphanumeric(TrimmedName) of
                        true ->
                            ok;
                        false ->
                            {error, "Name contains invalid characters. Only letters, numbers, underscores, and hyphens are allowed"}
                    end
            end
    end.

has_space(Name) ->
    lists:member($\s, Name).

is_alphanumeric(Name) ->
    lists:all(fun(Char) -> is_allowed_char(Char) end, Name).

is_allowed_char(Char) ->
    (Char >= $a andalso Char =< $z) orelse
    (Char >= $A andalso Char =< $Z) orelse
    (Char >= $0 andalso Char =< $9) orelse
    Char =:= $_ orelse
    Char =:= $-.

handle_game_interaction(Socket,ClientName) -> 
  gen_tcp:send(Socket, "Hello " ++ string:trim(ClientName) ++ "! You are now connected to the game!" ++ ?ACTION_SELECTION),
  receive_game_action(Socket,ClientName).

receive_game_action(Socket, ClientName) -> 
    case gen_tcp:recv(Socket, 0) of
        {ok, Action} ->
            ActionStr = string:trim(binary:bin_to_list(Action)),
            io:format("Received action: ~s from client: ~s~n", [ActionStr, ClientName]),
            case string:tokens(ActionStr, " ") of 
                ["create_room", RoomName] ->
                    create_room(Socket, RoomName, ClientName, false),
                    receive_game_action(Socket, ClientName);

                ["create_private_room", RoomName] ->
                    create_room(Socket, RoomName, ClientName, true),
                    receive_game_action(Socket, ClientName);

                ["invite_user", RoomName, TargetUser] ->
                    invite_user(Socket, RoomName, ClientName, TargetUser),
                    receive_game_action(Socket, ClientName);
                
                ["list_rooms"] ->
                    list_rooms(Socket, ClientName),
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
                    case string:trim(Message) of
                            "" ->
                                gen_tcp:send(Socket, "Error: Message cannot be empty.\n"),
                                receive_game_action(Socket, ClientName);
                            _ ->
                                send_message(Socket, RoomName, ClientName, Message),
                                receive_game_action(Socket, ClientName)
                    end;

                ["send_private_message", TargetUser | MessageRest] ->
                    Message = string:join(MessageRest, " "),
                    send_private_message(Socket, TargetUser, ClientName, Message),
                    receive_game_action(Socket, ClientName);

                ["quit"] -> 
                    gen_tcp:send(Socket, "Goodbye!\n"),
                    gen_tcp:close(Socket),
                    ok;

                 ["ban_user", RoomName, TargetUser] ->
                    ban_user(Socket, RoomName, ClientName, TargetUser),
                    receive_game_action(Socket, ClientName);

                ["ban_all", RoomName] ->
                    ban_all_users(Socket, RoomName, ClientName),
                    receive_game_action(Socket, ClientName);

                ["GAMEOVER"] ->
                    case ClientName of
                        ?ADMIN_USERNAME ->
                            io:format("Server shutting down on GAMEOVER command from admin.~n"),
                            gen_tcp:send(Socket, "Server is shutting down as requested by admin.\n"),
                            gen_tcp:close(Socket),
                            stop_server();
                        _Other ->
                            gen_tcp:send(Socket, "You have no power here!\n"),
                            io:format("Unauthorized GAMEOVER command attempted by client: ~s~n", [ClientName]),
                            receive_game_action(Socket, ClientName)
                    end;

                _OtherCommand ->
                    gen_tcp:send(Socket, "Unknown command. " ++ ?ACTION_SELECTION),
                    receive_game_action(Socket, ClientName)
            end;
        {error, closed} ->
            io:format("Client ~s disconnected.~n", [ClientName]),
            ets:delete(active_users, ClientName);
        {error, Reason} ->
            io:format("Error receiving action from ~s: ~p~n", [ClientName, Reason])
    end.

stop_server() ->
    io:format("Cleaning up resources and shutting down server.~n"),
    ActiveUsers = ets:tab2list(active_users),
    [gen_tcp:close(Socket) || {_ClientName, Socket} <- ActiveUsers],
    ets:delete_all_objects(rooms_table),
    ets:delete_all_objects(active_users),
    application:stop(my_game_app).

create_room(Socket, RoomName, ClientName, IsPrivate) ->
    case validate_room_name(RoomName) of
        ok ->
            RoomKey = list_to_binary(RoomName),
            TrimmedName = string:trim(ClientName),
            case ets:lookup(rooms_table, RoomKey) of
                [] ->
                    Room = #room{name = RoomName, users = [TrimmedName], creator = TrimmedName, private = IsPrivate, invited_users = []},
                    ets:insert(rooms_table, {RoomKey, Room}),
                    gen_tcp:send(Socket, "Room created successfully!\n");
                _ ->
                    gen_tcp:send(Socket, "Room already exists.\n")
            end;
        {error, Reason} ->
            gen_tcp:send(Socket, "Invalid room name: " ++ Reason ++ "\n")
    end.

validate_room_name(Name) ->
    TrimmedName = string:trim(Name),
    case true of
        _ when TrimmedName =:= "" ->
            {error, "Room name cannot be empty"};
        _ when length(TrimmedName) > 20 ->
            {error, "Room name must not exceed " ++ integer_to_list(20) ++ " characters"};
        _ ->
            case has_space(TrimmedName) of
                true ->
                    {error, "Room name must not contain spaces"};
                false ->
                    case is_alphanumeric(TrimmedName) of
                        true ->
                            ok;
                        false ->
                            {error, "Room name contains invalid characters. Only letters, numbers, underscores, and hyphens are allowed"}
                    end
            end
    end.

invite_user(Socket, RoomName, ClientName, TargetUser) ->
    RoomKey = list_to_binary(RoomName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, creator = Creator, private = true, invited_users = InvitedUsers} = Room}] ->
            case ClientName =:= Creator of
                true ->
                    case ets:lookup(active_users, TargetUser) of
                        [] ->
                            gen_tcp:send(Socket, "Error: User '" ++ TargetUser ++ "' does not exist or is not online.\n");
                        _ ->
                            case TargetUser =:= Creator of
                                true ->
                                    gen_tcp:send(Socket, "Error: You cannot invite yourself to the room you created.\n");
                                false ->
                                    UpdatedRoom = Room#room{invited_users = lists:usort([TargetUser | InvitedUsers])},
                                    ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
                                    send_private_message(Socket, TargetUser, "SERVER", "You have been invited to the private room '" ++ Name ++ "' by " ++ ClientName ++ ".")
                            end
                    end;
                false ->
                    gen_tcp:send(Socket, "Error: Only the creator of the private room can invite users.\n")
            end;
        [{_Key, #room{private = false}}] ->
            gen_tcp:send(Socket, "Error: You cannot invite users to a public room.\n");
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

list_rooms(Socket, ClientName) ->
    Rooms = ets:tab2list(rooms_table),
    PublicRooms = [
        describe_room(Room) || 
        {_RoomName, #room{private = false, banned_users = BannedUsers} = Room} <- Rooms,
        not lists:member(ClientName, BannedUsers)
    ],
    PrivateRooms = [
        describe_room(Room) || 
        {_RoomName, #room{private = true, users = Users, banned_users = BannedUsers} = Room} <- Rooms,
        lists:member(ClientName, Users),
        not lists:member(ClientName, BannedUsers)
    ],
    CreatorRooms = [
        describe_room(Room) || 
        {_RoomName, #room{creator = Creator} = Room} <- Rooms,
        Creator =:= ClientName
    ],
    VisibleRooms = PublicRooms ++ PrivateRooms ++ CreatorRooms,

    case VisibleRooms of
        [] ->
            gen_tcp:send(Socket, "No available rooms.\n");
        _ ->
            gen_tcp:send(Socket, "Available rooms:\n" ++ string:join(VisibleRooms, "\n\n") ++ "\n")
    end.

describe_room(#room{name = Name, users = Users, creator = Creator}) ->
    SortedUsers = lists:sort(Users),  
    io_lib:format("Room Name: ~s\nCreator: ~s\nUsers: ~s", 
                  [Name, Creator, string:join(SortedUsers, ", ")]).

list_users(Socket, ClientName) ->
    AllUsers = ets:tab2list(active_users),  
    OtherUsers = [User || {User, _Socket} <- AllUsers, User =/= ClientName],  
    case OtherUsers of
        [] ->
            gen_tcp:send(Socket, "No other active users.\n");
        _ ->
            gen_tcp:send(Socket, "Active users:\n" ++ string:join(OtherUsers, "\n") ++ "\n")
    end.

join_room(Socket, RoomName, ClientName) ->
    RoomKey = list_to_binary(RoomName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, users = Users, banned_users = BannedUsers, private = IsPrivate, invited_users = InvitedUsers} = Room}] ->
            case lists:member(ClientName, BannedUsers) of
                true ->
                    gen_tcp:send(Socket, "You are banned from the room '" ++ Name ++ "' and cannot join.\n");
                false ->
                    case lists:member(ClientName, Users) of
                        true ->
                            gen_tcp:send(Socket, "You are already a member of the room '" ++ Name ++ "'.\n");
                        false ->
                            case IsPrivate of
                                true ->
                                    case lists:member(ClientName, InvitedUsers) of
                                        true ->
                                            add_user_to_room(Socket, RoomKey, Room, Name, Users, ClientName);
                                        false ->
                                            gen_tcp:send(Socket, "Error: You are not invited to the private room '" ++ Name ++ "'.\n")
                                    end;
                                false ->
                                    add_user_to_room(Socket, RoomKey, Room, Name, Users, ClientName)
                            end
                    end
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

add_user_to_room(Socket, RoomKey, Room, Name, Users, ClientName) ->
    UpdatedRoom = Room#room{users = lists:usort([ClientName | Users])},
    ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
    OtherUsers = lists:delete(ClientName, Users),
    broadcast_message(OtherUsers, Name, "SERVER", ClientName ++ " has joined the room."),
    gen_tcp:send(Socket, "You joined the room '" ++ Name ++ "' successfully.\n").

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
                    OtherUsers = lists:delete(ClientName, Users),
                    broadcast_message(OtherUsers, Name, "SERVER", ClientName ++ " has left the room."),
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
        [{_Key, #room{name = Name, users = Users, creator = Creator} = _}] ->
            case TrimmedName of
                Creator ->
                    ets:delete(rooms_table, RoomKey),
                    OtherUsers = lists:delete(TrimmedName, Users),
                    broadcast_message(OtherUsers, RoomName, "SERVER", "This room has been deleted by its creator " ++ ClientName),
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
                    OtherUsers = lists:delete(TrimmedName, Users),
                    broadcast_message(OtherUsers, Name, TrimmedName, Message),
                    gen_tcp:send(Socket, "Message sent to all users in the room '" ++ Name ++ "'.\n");
                false ->
                    gen_tcp:send(Socket, "You are not a member of the room '" ++ Name ++ "'. Join the room to send a message.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

send_private_message(Socket, TargetUser, Sender, Message) ->
  case TargetUser =:= Sender of
        true ->
            gen_tcp:send(Socket, "Error: You cannot send a private message to yourself.\n");
        false ->
            case ets:lookup(active_users, TargetUser) of
                [{_TargetUser, TargetSocket}] ->
                    FormattedMessage = io_lib:format("[PRIVATE MESSAGE FROM ~s]: ~s\n", [Sender, Message]),
                    gen_tcp:send(TargetSocket, lists:flatten(FormattedMessage)),
                    gen_tcp:send(Socket, "Message sent successfully to " ++ TargetUser ++ ".\n");
                [] ->
                    gen_tcp:send(Socket, "User [" ++ TargetUser ++ "] is offline, the user won't read the message.\n")
            end
    end.

ban_user(Socket, RoomName, ClientName, TargetUser) ->
    RoomKey = list_to_binary(RoomName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, creator = Creator, users = Users, invited_users = InvitedUsers, banned_users = BannedUsers} = Room}] ->
            case ClientName =:= Creator of
                true ->
                    case lists:member(TargetUser, Users) orelse lists:member(TargetUser, InvitedUsers) of
                        true ->
                            UpdatedUsers = lists:delete(TargetUser, Users),
                            UpdatedInvited = lists:delete(TargetUser, InvitedUsers),
                            UpdatedBanned = lists:usort([TargetUser | BannedUsers]),
                            UpdatedRoom = Room#room{users = UpdatedUsers, invited_users = UpdatedInvited, banned_users = UpdatedBanned},
                            ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
                            ban_single_user(Name,TargetUser),
                            OtherUsers = lists:delete(Creator, UpdatedUsers),
                            broadcast_message(OtherUsers, Name, "SERVER", "User '" ++ TargetUser ++ "' has been banned from the room."),
                            gen_tcp:send(Socket, "User '" ++ TargetUser ++ "' has been banned from the room '" ++ Name ++ "'.\n");
                        false ->
                            gen_tcp:send(Socket, "User '" ++ TargetUser ++ "' is not in the room or invited.\n")
                    end;
                false ->
                    gen_tcp:send(Socket, "Only the creator of the room can ban users.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

ban_all_users(Socket, RoomName, ClientName) ->
    RoomKey = list_to_binary(RoomName),
    case ets:lookup(rooms_table, RoomKey) of
        [{_Key, #room{name = Name, creator = Creator, users = Users, invited_users = InvitedUsers, banned_users = BannedUsers} = Room}] ->
            case ClientName =:= Creator of
                true ->
                    ToBan = [User || User <- (Users ++ InvitedUsers), User =/= Creator],
                    UpdatedRoom = Room#room{users = [], invited_users = [], banned_users = lists:usort(ToBan ++ BannedUsers)},
                    ets:insert(rooms_table, {RoomKey, UpdatedRoom}),
                    [ban_single_user(Name, TargetUser) || TargetUser <- ToBan],
                    gen_tcp:send(Socket, "All users have been banned from the room '" ++ Name ++ "'.\n");
                false ->
                    gen_tcp:send(Socket, "Only the creator of the room can ban all users.\n")
            end;
        [] ->
            gen_tcp:send(Socket, "Room '" ++ RoomName ++ "' does not exist.\n")
    end.

ban_single_user(RoomName, TargetUser) ->
    case ets:lookup(active_users, TargetUser) of
        [{_TargetUser, TargetSocket}] ->
            gen_tcp:send(TargetSocket, "You have been banned by the room '" ++ RoomName ++ "'.\n");
        [] ->
            io:format("User ~s is offline, ban applied silently.~n", [TargetUser])
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
