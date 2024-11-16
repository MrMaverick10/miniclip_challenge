# My Game Project

## Description
"My Game" is an Erlang application designed to demonstrate the use of modules and basic testing. The application includes:
- A function that returns a welcome message (`who_are_you/0`).
- A function for basic mathematical operations (`do_some_math/5`).

---

## Build, Compilation and Test Instructions

### Requirements
- **Erlang/OTP**: Version 27 or higher.
- **Rebar3**: For project management in Erlang. If it is not in your system's `PATH`, you can execute it using the full path (e.g., `escript <path_to_rebar3>/rebar3` followed by `compile`, `eunit`, `release` and so on).

### Steps
1. Download or clone this repository
2. Navigate to the project's root directory. 
   cd <root-folder>/my_game/
3. Compile the project: 
   rebar3 compile
  
   If successful, you should see a message like:
   
  ===> Verifying dependencies...
  ===> Analyzing applications...
  ===> Compiling my_game

4. To run the unit tests, execute the following command:
   cd <root-folder>/my_game/
   rebar3 eunit

   You should see something like this: 

   ===> Verifying dependencies...
   ===> Analyzing applications...
   ===> Compiling my_game
   ===> Performing EUnit tests...
   ..
   Finished in 0.048 seconds
   2 tests, 0 failures

## Release Instructions

### Steps

1. Generate the release by running:
   rebar3 release

   If successful, you should see a message like:

  ===> Verifying dependencies...
  ===> Analyzing applications...
  ===> Compiling my_game
  ===> Assembling release my_game-0.1.0...
  ===> Release successfully assembled: _build/default/rel/my_game

  After the release is generated, it will be located in:
  <root-folder>/my_game/_build/default/rel/my_game/
2. To run the release in interactive mode (console):

   cd  <root-folder>/my_game/_build/default/rel/my_game/bin
   .\my_game console  

3. Once inside the Erlang shell, test the main functions:

   1> my_game_app:who_are_you().
    "Hello, i am a new game!"
   
   2> my_game_app:do_some_math(10, 5, 3, 2, 6).
   4.0

---

## Functions
### who_are_you/0
-This function returns a welcome message
Example:
my_game_app:who_are_you(). %% Result: "Hello, i am a new game!"

### do_some_math/5
-This function performs a mathematical operation on five numbers using the formula ((A + B) - C) * D / E
Example:
my_game_app:do_some_math(10, 5, 3, 2, 6). %% Result: 4.0
