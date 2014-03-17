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

-spec new() -> db_ref().
new() -> [].

-spec destroy(db_ref()) -> ok.
destroy(_) -> ok.

-spec write(atom(), term(), db_ref()) -> db_ref().
write(Key, Value, Db) -> write(Key, Value, Db, []).
-spec write(atom(), term(), db_ref(), db_ref()) -> db_ref().
write(Key, Value, [], Db) -> [{Key, Value} | Db];
write(Key, Value, [Next|Db], NewDb) ->
  case Next of
    {Key, _} -> write(Key, Value, Db, NewDb);
    _ -> write(Key, Value, Db, [Next|NewDb])
  end.

-spec delete(atom(), db_ref()) -> db_ref().
delete(Key, Db) -> delete(Key, Db, []).
-spec delete(atom(), db_ref(), db_ref()) -> db_ref().
delete(Key, [], NewDb) -> NewDb;
delete(Key, [Next|Db], NewDb) ->
  case Next of
    {Key, _} -> delete(Key, Db, NewDb);
    _ -> delete(Key, Db, [Next|NewDb])
  end.

-spec read(atom(), db_ref()) -> {ok, term()} | {error, instance}.
read(Key, []) -> {error, instance};
read(Key, [Next | Db]) ->
  case Next of
    {Key, Value} -> {ok, Value};
    _ -> read(Key, Db)
  end.

-spec match(term(), db_ref()) -> [atom()].
match(Value, Db) -> match(Value, Db, []).
-spec match(term(), db_ref(), [atom()]) -> [atom()].
match(Value, [], Results) -> Results;
match(Value, [Next|Db], Results) ->
  case Next of
    {Key, Value} -> match(Value, Db, [Key | Results]);
    _ -> match(Value, Db, Results)
  end.

