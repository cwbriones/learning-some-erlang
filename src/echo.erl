-module(echo).
-export([
    start/0,
    stop/0,
    print/1,
    init/0
  ]).

-spec start() -> ok.
start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  ok.

-spec stop() -> ok.
stop() ->
  whereis(?MODULE) ! {stop, undefined},
  ok.

-spec print(term()) -> ok.
print(Msg) ->
  whereis(?MODULE) ! {print, Msg},
  ok.

-spec init() -> ok.
init() -> loop().

loop() ->
  receive
    {print, Msg} -> 
      io:format("~p~n", [Msg]),
      loop();
    {stop, undefined} -> ok;
    _ -> undefined
  end.
