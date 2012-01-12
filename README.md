seqbind
=======


seqbind is a simple parse transformation for Erlang that helps escaping the problem of sequential naming of variables (like `Name`, `Name1`, `Name2`, ...)

In order to use it, one should enable `seqbind` parse transformation, either through compiler options or in the module:

```erlang
-compile({parse_transform,seqbind}).
```

Sequantial bindings are following the convention of `Name@`. This is the only way to use sequential bindings.

Example
-------

```erlang
myfun() ->
   A@ = 1,
   A@ = A@ + 1,
   A@.
```

This function will return two.

You can also match (notice that for matching you need to drop the `@` suffix):

```erlang
myfun() ->
   A@ = 1,
   1 = A,
   A = 1.

Sequential bindings can also be used in function arguments.