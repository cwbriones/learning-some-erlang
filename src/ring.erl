-module(ring).
-export([
    start/1,
    stop/0,
    send/2,
    init/2
  ]).

-record(state, {next, id}).

-spec start(pos_integer()) -> ok.
start(N) when N > 1 ->
  %% Spawn the parent process
  register(?MODULE, spawn(?MODULE, init, [1, N])),
  ok.

-spec stop() -> ok.
stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    Pid -> Pid ! 
      {stop, undefined, undefined},
      ok
  end.

-spec init(pos_integer(), pos_integer()) -> ok.
init(N, Max) -> 
  io:format("Spawning Process ~p~n", [N]),
  NextPid = 
  case N < Max of
    false -> whereis(?MODULE);
    true  -> spawn(?MODULE, init, [N+1, Max])
  end,
  loop(#state{next=NextPid, id=N}),
  ok.

-spec send(term(), pos_integer()) -> ok.
send(Msg, M) when M > 0 -> 
  whereis(?MODULE) ! {send, Msg, M-1},
  ok.

-spec loop(#state{}) -> ok.
loop(State = #state{next=NextPid, id=Id}) ->
  receive
    {send, Msg, MsgCount} ->
      io:format("Process ~p Got: ~p~n", [Id, Msg]),
      case MsgCount of
        0 -> nothing_to_do;
        _ -> NextPid ! {send, Msg, MsgCount - 1}
      end,
      loop(State);
    {stop, undefined, undefined} ->
      io:format("Process ~p Exiting~n", [Id]),
      NextPid ! {stop, undefined, undefined},
      ok
  end.
