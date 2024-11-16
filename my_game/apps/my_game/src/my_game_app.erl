-module(my_game_app).
-behaviour(application).
-export([start/2, stop/1,who_are_you/0,do_some_math/5]).

start(_StartType, _StartArgs) ->
    io:format("My game is working!~n"),
    my_game_sup:start_link().

stop(_State) ->
    ok.

%%test function

who_are_you() ->
    "Hello, i am a new game!".

do_some_math(A, B, C, D, E) -> 
    (((A + B) - C) * D) / E.
