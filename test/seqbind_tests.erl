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

complex_assignment_test() ->
    {ok, A@} = {ok, element(1,{1})},
    {ok, A@} = {ok, element(1,{2})},
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


in_arg_test() ->
    A@ = 1,
    ?assertEqual(2,arg(A@)).

in_multi_arg_test() ->
    A@ = 1,
    ?assertEqual(3,multi_arg(A@,A@,A@)).

multi_arg(A,B,C) ->
    A + B + C.

case_test() ->
    case 1 of
        A@ ->
            ok;
        2=A@ ->
            ok
    end,
    ?assertEqual(1,A@).

case_stack_test() ->    
    ?assertEqual(2,case_stack(1)).

case_stack(Req@) ->
    case Req@ of 
        Req@ ->
            Req@ = Req@ +1
    end,
    Req@.

if_stack_test() ->    
    ?assertEqual(2,if_stack(1)).

if_stack(Req@) ->
    if true ->
            Req@ = Req@ + 1
    end,
    Req@.
            
            
