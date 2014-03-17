-module(db).
-export([
    new/0, 
    destroy/1, 
    write/3, 
    delete/2, 
    read/2, 
    match/2
  ]).

-type db_ref() :: [{atom(), term()}].

-spec start() -> ok.
start() ->
  spawn(?MODULE_NAME, init, []),
  ok.

-spec init() -> ok.
init() -> loop(nil).

-spec loop(tree()) -> ok.
loop(Tree = #treenode{}) ->
  receive
    {sync, {From, Ref}, Msg} -> loop(Tree);
    {sync, _, stop} -> ok
  end.

-spec new() -> db_ref().
new() -> [].

-spec destroy(db_ref()) -> ok.
destroy(_) -> ok.

-spec write(atom(), term(), db_ref()) -> db_ref().

-spec delete(atom(), db_ref()) -> db_ref().

-spec read(atom(), db_ref()) -> {ok, term()} | {error, instance}.

-spec match(term(), db_ref()) -> [atom()].

