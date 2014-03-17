-module(sort).
-export([
    mergesort/1,
    qsort/1
  ]).

%% Quicksort Implementation

-spec qsort(list()) -> list().
qsort([]) -> [];
qsort([H]) -> [H];
qsort([X|Xs]) ->
  {Less, More} = partition(X, Xs, [], []),
  mylists:concatenate([qsort(Less), [X|qsort(More)]]).

-spec partition(term(), list(), list(), list()) -> {[integer()], [integer()]}.
partition(_, [], Less, More) -> {Less, More};
partition(Pivot, [X|Xs], Less, More) ->
  case X > Pivot of
    false -> partition(Pivot, Xs, [X|Less], More);
    true  -> partition(Pivot, Xs, Less, [X|More])
  end.

%% Mergesort implementation

-spec mergesort(list()) -> list().
mergesort([]) -> [];
mergesort([X]) -> [X];
mergesort(Items) ->
  SplitAt = mylists:length(Items) div 2,
  {Front, Back} = mylists:split_at(SplitAt, Items),
  SortedFront = mergesort(Front),
  SortedBack  = mergesort(Back),
  io:format("Split: ~p Front: ~p, Back: ~p~n", [SplitAt, SortedFront, SortedBack]),
  merge(SortedFront, SortedBack, []).

-spec merge(list(), list(), list()) -> list().
merge([], [], Result) -> mylists:reverse(Result);
merge([X|Xs], [], Result) ->
  merge(Xs, [], [X|Result]);
merge([], [Y|Ys], Result) ->
  merge([], Ys, [Y|Result]);
merge([X|Xs], [Y|Ys], Result) ->
  case X > Y of
    false -> merge(Xs, [Y|Ys], [X|Result]);
    true  -> merge([X|Xs], Ys, [Y|Result])
  end.
