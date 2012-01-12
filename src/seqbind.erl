-module(seqbind).
-export([parse_transform/2]).

-record(state, 
        {
          f,
          seqvars = [],
          side,
          options,
          in_fun,
          skip_clause
        }).

parse_transform(Forms, Options) ->
    {Forms1, _State} = parse_trans:transform(fun do_transform/4, 
                                             #state{ options = Options },
                                             Forms, Options),
    Result = parse_trans:revert(Forms1),
%    io:format("~p~n",[Result]),
    Result.

do_transform(function, {function, _Line, Name, Arity, _}=Form, _Context, #state{} = State) ->
    {Form, true, State#state{ f = {Name, Arity} }};
do_transform(clause, Form, _Context, #state{ skip_clause = true } = State) ->
    {Form, true, State#state{ skip_clause = false }};
do_transform(clause, Form, _Context, #state{ seqvars = SeqVars, options = Options } = State) ->
    {Form1, State1} = parse_trans:do_transform(fun do_transform/4,
                                               State#state{ skip_clause = true, seqvars = [[]|SeqVars] },
                                               [Form], 
                                               Options),
    {hd(parse_trans:revert(Form1)), false, State1#state{ seqvars = maybe_tl(State1#state.seqvars) }};
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
                    do_transform(variable, Form, Context, State#state{ seqvars = R })
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
