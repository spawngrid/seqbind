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

match_test() ->
    A@ = 1 + 1,
    2 = A,
    A = 2,
    2 = A@.
    
 
