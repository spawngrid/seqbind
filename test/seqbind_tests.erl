-module(seqbind_tests).
-compile({parse_transform,seqbind}).
-include_lib("eunit/include/eunit.hrl").

multiple_assignments_test() ->
    A@ = 1,
    A@ = 2,
    ?assertEqual(2,A@).

multiple_assignments_with_use_test() ->
    A@ = 1,
    A@ = A@ + 1,
    ?assertEqual(2,A@).

multiple_variables_test() ->
    A@ = 1,
    A@ = A@ + 1,
    B@ = 2,
    B@ = A@ + B@,
    ?assertEqual(4,B@).

match_test() ->
    A@ = 1 + 1,
    2 = A,
    A = 2,
    2 = A@.

fun_test() ->    
    A@ = 1,
    F = fun() ->
            A@
        end,
    ?assertEqual(1,F()).

fun_arg_test() ->    
    A@ = 1,
    F = fun(A@) ->
            A@ + 1
        end,
    ?assertEqual(3,F(2)).

fun_override_test() ->    
    A@ = 1,
    F = fun() ->
                A@ = A@ + 1,
                A@
        end,
    ?assertEqual(2,F()).

arg_test() -> 
    ?assertEqual(2,arg(1)).

arg(A@) ->
    A@ + 1.
