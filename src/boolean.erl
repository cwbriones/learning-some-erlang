-module(boolean).
-export([b_not/1, b_and/2, b_or/2]).

-spec b_not(boolean()) -> boolean().
b_not(true) -> false;
b_not(false) -> true.

-spec b_or(boolean(), boolean()) -> boolean().
b_or(true, _) -> true;
b_or(_, true) -> true;
b_or(false, false) -> false.

-spec b_and(boolean(), boolean()) -> boolean().
b_and(false, _) -> false;
b_and(_, false) -> false;
b_and(true, true) -> true.
