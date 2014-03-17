-module(cross).
-export([
    start/1,
    stop/0,
    send/2,
    init/2
  ]).

-record(state, {next, id, alt}).

-spec start(pos_integer()) -> ok.
start(N) when N > 2 ->
  %% Spawn the parent process
  register(?MODULE, spawn(?MODULE, init, [1, N])),
  ok.

-spec init(pos_integer(), pos_integer()) -> ok.
init(N, Max) -> 
  io:format("Spawning Process ~p~n", [N]),
  NextPid = 
  case N =:= Max orelse N =:= (Max div 2) + 1 of
    true -> whereis(?MODULE);
    false  -> spawn(?MODULE, init, [N+1, Max])
  end,

  OtherPid =
  case N =:= 1 of
    false -> NextPid;
    true  -> spawn(?MODULE, init, [(Max div 2) + 2, Max])
  end,

  loop(#state{next=NextPid, id=N, alt=OtherPid}),
  ok.

-spec stop() -> ok.
stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    Pid ->
      Pid ! {stop, undefined, undefined, false},
      ok
  end.

-spec send(term(), pos_integer()) -> ok.
send(Msg, M) when M > 0 -> 
  case whereis(?MODULE) of
    undefined -> ok;
    Pid -> Pid ! {send, Msg, M-1, false}
  end,
  ok.

-spec loop(#state{}) -> ok.
loop(State = #state{next=Pid, id=Id, alt=AltPid}) ->
  receive
    {send, Msg, MsgCount, UseAlt} ->
      NextPid =
      case UseAlt of
        true -> AltPid;
        false -> Pid
      end,
      NextUse =
      case Id =:= 1 of
        true -> not UseAlt;
        false -> UseAlt
      end,
      io:format("Process ~p Got: ~p~n", [Id, Msg]),
      case MsgCount of
        0 -> nothing_to_do;
        _ -> NextPid ! {send, Msg, MsgCount - 1, NextUse}
      end,
      loop(State);
    {stop, undefined, undefined, UseAlt} ->
      NextPid =
      case UseAlt of
        true -> AltPid;
        false -> Pid
      end,
        
      NextPid ! {stop, undefined, undefined, true},

      case UseAlt of
        false -> loop(State);
        _ -> 
          io:format("Process ~p Exiting~n", [Id]),
          ok
      end
  end.
