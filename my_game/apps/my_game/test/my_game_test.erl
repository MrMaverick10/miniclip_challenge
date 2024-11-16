-module(my_game_test).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual("Hello, i am a new game!", my_game_app:who_are_you()).

calc_test() ->
    ?assertEqual(12.0, my_game_app:do_some_math(5, 4, 3, 2, 1)).