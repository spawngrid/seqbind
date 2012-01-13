seqbind
=======

Problem
-------

Does it bother you that some of your code looks like this?

```erlang
 L1 = lists:map(fun (X) -> ... end, L),
 L2 = lists:filter(fun (X) -> ... end, L1)
 %% or
 {Q,Req1} = cowboy_http_req:qs_val(<<"q">>,Req),
 {Id,Req2} = cowboy_http_req:qs_val(<<"id">>,Req1)
```

Solution
--------

While there are some solutions available, like giving variables more descriptive names or extracting every step to a separate function, they might not be suitable for every case. Particularly, giving descriptive names still doesn't help with reordering and having too many function calls might add a slight overhead or increase the complexity of the code.

SeqBind offers a different, yet simple, solution to this problem.

It introduces a concept of sequential bindings. What is that? Sequential binding is a binding that has a suffix of `@`, like `L@` or `Req@`. 

SeqBind is a parse transformation that numbers all occurences of such bindings, so they become `L@0`,`L@1`,`Req@0`,`Req@1` and so on.

In order to use it, one should enable `seqbind` parse transformation, either through compiler options or in the module:

```erlang
-compile({parse_transform,seqbind}).
```

How
---

There are few rules to be followed.

### Left and Right


In the matches (such as `A@ = 1`) the side of the match is significant to SeqBind. Even though in Erlang itself it is not, this agreement allows seqbind to do its job.

The basic idea is that if you have a sequential binding on the left, its counter will be incremented. If it's on the right, the current counter value will be used.

As a consequence of this, one has to understand, that commonly used syntax of

```erlang
#state{} = State@
```

is not a good idea as it will not increment the counter for `State@` (with the exception of when this syntax is used in a function clause). In order to achieve desired result, one should do

```erlang
State@ = #state{}
```

### Matching

If instead of incrementing a sequential binding's counter you actually want to match to its value while putting it on the left side, you should simply drop the suffix in order to be able to do so:


```erlang
State@ = get_state(),
State = get_state_again()
```

The same goes to `case`,`if` and `receive` clauses.