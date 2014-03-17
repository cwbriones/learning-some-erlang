-module(side_effects).
-export([print/1, print_even/1]).

print(N) when N >= 1 -> print(1, N).

print(N, Max) when N > Max -> ok;
print(N, Max) ->
  io:format("Number: ~p~n", [N]),
  print(N + 1, Max).

print_even(N) when N >= 2 -> print_even(2, N).
print_even(N, Max) when N > Max -> ok;
print_even(N, Max) ->
  io:format("Number: ~p~n", [N]),
  print_even(N + 2, Max).
