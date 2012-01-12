-module(seqbind).
-export([parse_transform/2]).

-record(state, 
        {
          f,
          seqvars = [],
          side,
          options,
          in_case = false,
          in_if = false,
          skip_clause,
          skip_case,
          skip_if,
          top_level_case_clause
        }).

parse_transform(Forms, Options) ->
    {Forms1, _State} = parse_trans:transform(fun do_transform/4, 
                                             #state{ options = Options },
                                             Forms, Options),
    Result = parse_trans:revert(Forms1),
%    io:format("~p~n",[Result]),
    Result.

do_transform(function, {function, _Line, Name, Arity, _}=Form, _Context, #state{} = State) ->
    {Form, true, State#state{ f = {Name, Arity}, seqvars = [] }};
do_transform(clause, {clause, Line, H, G, B} = Form, _Context,
             #state{ in_case = true,
                     top_level_case_clause = true,
                     options = Options } = State) ->
    {H1, State1} = parse_trans:do_transform(fun do_transform/4,
                                               State#state{ 
                                                 side = left,
                                                 top_level_case_clause = false
                                                },
                                               H, 
                                               Options),
    {{clause, Line, parse_trans:revert(H1), G, B}, true, State1#state { side = undefined }};
do_transform(clause, Form, _Context, #state{ skip_clause = true } = State) ->
    {Form, true, State#state{ skip_clause = false, top_level_case_clause = true }};
do_transform(clause, Form, _Context, #state{ seqvars = SeqVars, 
                                             in_case = InCase,
                                             in_if = InIf,
                                             options = Options } = State) ->
    case (InCase or InIf) of
        true ->
            SeqVars1 = SeqVars;
        false ->
            SeqVars1 = [[]|SeqVars]
    end,
    {Form1, State1} = parse_trans:do_transform(fun do_transform/4,
                                               State#state{ skip_clause = true, seqvars = SeqVars1 },
                                               [Form], 
                                               Options),
    State2 = case (InCase or InIf) of
                 true ->
                     State1#state{ seqvars = State1#state.seqvars };
                 _ ->
                     State1#state{ seqvars = maybe_tl(State1#state.seqvars) }
             end,
    {hd(parse_trans:revert(Form1)), false, State2};
do_transform(case_expr, Form, _Context, #state{ skip_case = true } = State) ->
    {Form, true, State#state{ skip_case = false }};
do_transform(case_expr, Form, _Context, #state{ options = Options}  = State) ->
    {Form1, State1} = parse_trans:do_transform(fun do_transform/4,
                                               State#state{ 
                                                 in_case = true,
                                                 skip_case = true
                                                },
                                               [Form], 
                                               Options),
    {hd(parse_trans:revert(Form1)), false, State1#state{ in_case = false, 
                                                         skip_case = false }};
do_transform(if_expr, Form, _Context, #state{ skip_if = true } = State) ->
    {Form, true, State#state{ skip_if = false }};
do_transform(if_expr, Form, _Context, #state{ options = Options}  = State) ->
    {Form1, State1} = parse_trans:do_transform(fun do_transform/4,
                                               State#state{ 
                                                 in_if = true,
                                                 skip_if = true
                                                },
                                               [Form], 
                                               Options),
    {hd(parse_trans:revert(Form1)), false, State1#state{ in_if = false, 
                                                         skip_if = false }};

do_transform(match_expr, {match,Line,L,R}, _Context,
             #state{ options = Options } = State) ->
    {RForm, State1} = parse_trans:do_transform(fun do_transform/4,
                                          State#state{ side = right },
                                          [R], Options),
    {LForm, State2} = parse_trans:do_transform(fun do_transform/4,
                                          State1#state{ side = left },
                                          [L], Options),
    {{match,Line,
     hd(parse_trans:revert(LForm)),
     hd(parse_trans:revert(RForm))},false,State2#state{ side = undefined }};
    
do_transform(variable, {var, Line, Name}=Form, Context, 
             #state{ seqvars = [SeqVars|R]=SV, side = Side } = State) ->
    case {binding_type(Name),proplists:get_value(Name, SeqVars), Side} of
        {{seq, CleanName},undefined,_} ->
            case R of
                [] ->
                    {{var, Line, seq_name(CleanName, 0)}, false, State#state{ seqvars = [[{Name,0}|SeqVars]|R] }};
                _ ->
                    {Form1, Rec, #state{ seqvars = [SeqVars1|_] } = State1 } = do_transform(variable, Form, Context, State#state{ seqvars = R }),
                    {Form1, Rec, State1#state{ seqvars = [SeqVars1 ++ SeqVars|R] }}
            end;
        {{normal, _}, undefined, _} ->
            SeqName = seq_name(Name),
            Lookup = [N || Seq <- SV,
                              {N, _C} <- Seq,
                              N =:= SeqName],
            case Lookup of
                [] ->
                    {Form, false, State};
                _ ->
                    Result0 = do_transform(variable, {var, Line, SeqName}, Context, State#state{ seqvars = SV, side = right }),
                    State1 = element(size(Result0),Result0),
                    setelement(size(Result0), Result0, State1#state{ side = State#state.side })
            end;
        {{seq, CleanName},Counter, left} ->
            {{var, Line, seq_name(CleanName, Counter + 1)}, false, State#state{ seqvars = [[{Name,Counter + 1}|SeqVars]|R] }};
        {{seq, CleanName},Counter, _} ->
            {{var, Line, seq_name(CleanName, Counter)}, false, State}
    end;
do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.

    
binding_type(Name) ->
    case lists:reverse(atom_to_list(Name)) of
        "@" ++ Clean ->
            {seq, list_to_atom(lists:reverse(Clean))};
        _ ->
            {normal, Name}
    end.

seq_name(Name, Ctr) ->
    list_to_atom(atom_to_list(Name) ++ integer_to_list(Ctr)).

seq_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "@").

maybe_tl([]) ->
    [];
maybe_tl([_|T]) ->
    T.
