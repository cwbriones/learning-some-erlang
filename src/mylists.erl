-module(mylists).
-export([
    length/1,
    split_at/2,
    filter/2,
    reverse/1,
    concatenate/1,
    flatten/1,
    test_list/0
  ]).

-spec length(list()) -> pos_integer().
length(Xs) -> length(Xs, 0).
-spec length(list(), pos_integer()) -> pos_integer().
length([], Len) -> Len;
length([X|Xs], Len) -> length(Xs, Len + 1).

-spec split_at(pos_integer(), list()) -> {list(), list()}.
split_at(N, Xs) -> split_at(N, Xs, []).
-spec split_at(pos_integer(), list(), list()) -> {list(), list()}.
split_at(_, [], Result) -> {reverse(Result), []};
split_at(0, Xs, Result) -> {reverse(Result), Xs};
split_at(N, [X|Xs], Result) -> split_at(N-1, Xs, [X|Result]).

-spec reverse([integer()]) -> [integer()].
reverse(List) -> reverse(List, []).
-spec reverse([integer()], [integer()]) -> [integer()].
reverse([], Reversed) -> Reversed;
reverse([H|List], Reversed) -> reverse(List, [H|Reversed]).

-spec filter([integer()], integer()) -> [integer()].
filter(List, N) -> filter(List, N, []).
-spec filter([integer()], integer(), [integer()]) -> [integer()].
filter([], N, Filtered) -> reverse(Filtered);
filter([H|List], N, Filtered) when H == N ->
  filter(List, N, Filtered);
filter([H|List], N, Filtered) ->
  filter(List, N, [H|Filtered]).

-spec concatenate([list()]) -> list().
concatenate([]) -> [];
concatenate([X|Xs]) -> concatenate(X, Xs, []).
-spec concatenate(list(), [list()], list()) -> list().
concatenate([X|Xs], Lists, Result) -> concatenate(Xs, Lists, [X|Result]);
concatenate([], [X|Xs], Result) -> concatenate(X, Xs, Result);
concatenate([], [], Result) -> reverse(Result).

-spec flatten([list()]) -> list().
flatten(Xs) -> mylists:reverse(flatten(Xs, [])).
-spec flatten([list()], list()) -> list().
flatten([], Acc) -> Acc;
flatten([X|Xs], Acc) when is_list(X) -> flatten(Xs, flatten(X, Acc));
flatten([X|Xs], Acc) -> flatten(Xs, [X|Acc]).

test_list() -> [[1, [2, [3], []]], [[[4]]], [5, 6]].
