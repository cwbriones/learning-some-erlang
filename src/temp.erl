-module(temp).
-export([f2c/1, c2f/1]).

-spec f2c(float()) -> float().
f2c(Fahrenheit) -> 5/9 * (Fahrenheit - 32.0).

-spec c2f(float()) -> float().
c2f(Celcius) -> 1.8 * Celcius + 32.0.
