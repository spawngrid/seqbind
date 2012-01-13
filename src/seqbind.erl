-module(seqbind).
-export([parse_transform/2]).

-record(state, 
        {
          scope = [],
          seqvars = [],
          clauses_seqvars = [],
          side,
          options
        }).

parse_transform(Forms, Options) ->
    {Forms1, _State} = parse_trans:transform(fun scope/4, 
                                             #state{ options = Options },
                                             Forms, Options),
    Result = parse_trans:revert(Forms1),
%    io:format("~s~n",[[ erl_pp:form(F) || F <- Result]]),
    Result.


-define(UnshiftSeqVars, [[]|SeqVars]).
-define(ShiftSeqVars(SV), maybe_tl(SV)).

transform(Fun, State, Form, Context) when is_tuple(Form) ->
    {L,Rec,State1} = transform(Fun, State, [Form], Context),
    {hd(L),Rec,State1};

transform(Fun, State, Forms, Context) when is_list(Forms) ->
    {Form1, State1} = parse_trans:do_transform(Fun,
                                               State,
                                               Forms, 
                                               Context),
    {parse_trans:revert(Form1),false,State1}.

-define(Scope(F,Name,ScopeName,In,Out), 
        scope(Name, Form, Context, #state{ 
                            seqvars = SeqVars,
                            scope = Scopes
                           } = State) ->
               {Form1, Rec, State1} = 
                   transform(fun F/4,
                             State#state{ scope = [ScopeName|Scopes],
                                          seqvars = In
                                        }, 
                             Form, Context),
               {Form1, Rec, State1#state{
                              seqvars = Out(State1#state.seqvars) 
                             }}).

-define(ScopeShift(F,Name,ScopeName),?Scope(F,Name,ScopeName,?UnshiftSeqVars,?ShiftSeqVars)).
-define(ScopeNoShift(F,Name,ScopeName),?Scope(F,Name,ScopeName,SeqVars,(fun (X) -> X end))).


?ScopeShift(do_transform,function, function);
?ScopeShift(do_transform,fun_expr, 'fun');
?ScopeNoShift(do_transform,receive_expr, 'receive');
?ScopeNoShift(do_transform,case_expr, 'case');
?ScopeNoShift(do_transform,if_expr, 'if');
scope(clause, {clause, Line, H, G, B}, Context, #state{
                      } = State) ->
    {H1, Rec, State1} = 
        transform(fun do_transform/4,
                  State#state{
                    side = left
                   }, 
                  H, Context),
    GsT = [
           transform(fun do_transform/4,
                     State#state{
                       seqvars = State1#state.seqvars
                      }, 
                     G0, Context) || G0 <- G ],
    case lists:reverse(GsT) of
        [] ->
            State2 = State1;
        [{_, _, State2}|_] ->
            ok
    end,

    G1 = [ G0 || {G0, _, _} <- GsT ],

    {B1, Rec, State3} = 
        transform(fun do_transform/4,
                  State#state{
                    seqvars = State2#state.seqvars
                   }, 
                  B, Context),

    {{clause, Line, H1, G1, B1}, Rec, State#state{
                                        clauses_seqvars = State3#state.seqvars
                                       }};
?ScopeShift(do_transform,clause, clause);
scope(match_expr, {match, Line, L, R}, Context, #state{ 
                                         scope = Scopes
                                        } = State) ->
    {R1, _Rec, State1} = 
        transform(fun do_transform/4,
                  State#state{ scope = [match|Scopes],
                               side = right
                             }, 
                  R, Context),
    {L1, _Rec, State2} = 
        transform(fun do_transform/4,
                  State#state{ scope = [match|Scopes],
                               side = left,
                               seqvars = State1#state.seqvars
                             }, 
                  L, Context),

    {{match, Line, L1, R1}, false, State#state{
                                   seqvars = State2#state.seqvars
                                    }};
    
scope(_Type, Form, _Context, State) ->
    {Form, true, State}.

-define(do_transform_scope(FormType,ScopeName),
do_transform(FormType, Form, Context, #state{ scope = [H|_] } = State) when H /= ScopeName ->
    {Form1, Rec, State1} = transform(fun scope/4, State, Form, Context),
    {Form1, Rec, State1#state{
                   seqvars = State1#state.clauses_seqvars,
                   clauses_seqvars = []}}).


do_transform(match_expr = Type, Form, Context, State) ->
    scope(Type, Form, Context, State);

do_transform(clause = Type, Form, Context, #state{ scope = [T|_] } = State)
  when T == 'case';
       T == 'if';
       T == 'receive' ->
    scope(Type, Form, Context, State);

?do_transform_scope(case_expr,'case');
?do_transform_scope(if_expr,'if');
?do_transform_scope(receive_expr,'receive');

do_transform(variable, {var, Line, Name}=Form, Context, 
             #state{ seqvars = SV, side = Side } = State) ->
    SeqVars = maybe_hd(SV),
    R = maybe_tl(SV),
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
%% Funny debug hack
do_transform(application, {call, _Line, {atom, _, seqbind_debug}, []}, _Context, State) ->
    io:format("===SEQBIND DEBUG===~n"
              "~p"
              "~n==================~n",[State]),
    {{atom, _Line, ok},false,State};
do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.


%%%
    
binding_type(Name) ->
    case lists:reverse(atom_to_list(Name)) of
        "@" ++ Clean ->
            {seq, list_to_atom(lists:reverse(Clean))};
        _ ->
            {normal, Name}
    end.

seq_name(Name, Ctr) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ integer_to_list(Ctr)).

seq_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "@").

maybe_tl([]) ->
    [];
maybe_tl([_|T]) ->
    T.

maybe_hd([]) ->
    [];
maybe_hd([H|_]) ->
    H.
