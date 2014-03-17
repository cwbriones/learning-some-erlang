-module(evaluate).
-export([sum/1, sum_interval/2, create/1, reverse_create/1]).

-spec sum(integer()) -> integer().
sum(N) -> N * (N + 1) / 2.

-spec sum_interval(integer(), integer()) -> integer().
sum_interval(N, M) -> sum(M) - sum(N).

-spec create(pos_integer()) -> [pos_integer()].
create(N) -> create(N, []).
-spec create(pos_integer(), [pos_integer()]) -> [pos_integer()].
create(0, List) -> List;
create(N, List) when N > 0 -> create(N-1, [N|List]).

-spec reverse_create(pos_integer()) -> [pos_integer()].
reverse_create(Max) when Max > 0 -> reverse_create(1, Max, []).

-spec reverse_create(pos_integer(), pos_integer(), [pos_integer()]) 
  -> [pos_integer()].
reverse_create(Max, Max, List) -> [Max | List];
reverse_create(N, Max, List) when N < Max -> reverse_create(N+1, Max, [N|List]).
