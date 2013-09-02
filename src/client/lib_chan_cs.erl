%% Copyright
-module(lib_chan_cs).
-author("Ron").

%% API
-export([]).
-compile(export_all).




start_raw_client(Host, Port, PacketLength) ->
  gen_tcp:connect(Host, Port,
    [binary,
      {active, true},
      {packet, PacketLength}
    ]
  )
.


start_raw_server(Port, Fun, Max, PacketLength) ->
  Name = port_name(Port),
  case whereis(Name) of
    undefined ->
      Self = self(),
      Pid = spawn_link(fun() ->
        cold_start(Self, Port, Fun, Max, PacketLength) end),
      receive
        {Pid, ok} ->
          register(Name, Pid),
          {ok, self()}     ;
        {Pid, Error} -> Error
      end      ;
    _Pid -> {error, already_started}

  end
.

stop(Port) when is_integer(Port) ->
  Name = port_name(Port),
  case whereis(Name) of
    undefined -> not_started;
    Pid -> exit(Pid, kill),
      (catch unregister(Name)),
      stopped
  end
.


children(Port) when is_integer(Port) ->
  port_name(Port) ! {children(), self()},
  receive
    {session_server, Reply} -> Reply
  end
.

port_name(Port) when integer(Port) ->
  list_to_atom("portServer" ++ integer_to_list(Port)).


cold_start(Master, Port, Fun, Max, PacketLength) ->
  process_flag(trap_exit, true),
  case gen_tcp:listen(Port,
    [binary,
      {nodelay, true},
      {packet, PacketLength},
      {reuseaddr, true},
      {active, true}]) of

    {ok, Listen} ->
      Master ! {self(), ok},
      New = start_accept(Listen, Fun),
      socket_loop(Listen, New, [], Fun, Max);

    Error ->
      Master ! {self(), Error}
  end
.

%% 管理连接，离开，接入，开启新连接
socket_loop(Listen, New, Active,[], Fun, Max) ->
  receive
    {istarted, New} ->
      Active1 = [New|Active],
      possibly_start_another(false, Listen, Active1, Fun, Max);

    {'EXIT', New, _Why} ->
      possibly_start_another(false, Listen, Active, Fun, Max);
    {'EXIT', Pid, _Why} ->
      Active1 = lists:delete(Pid, Active),
      possibly_start_another(false, Listen, Active1, Fun, Max);
    {children, From} ->
      From ! {session_server, Active},
      socket_loop(Listen, New, Active, Fun, Max);
    _Other ->
      socket_loop(Listen, New, Active, Fun, Max)

  end
.

possibly_start_another(New, Listen, Active, Fun, Max) when pid(New) ->
  socket_loop(Listen, New, Active, Fun, Max);

possibly_start_another(false, Listen, Active, Fun, Max) ->
  case length(Active) of
    N when N < Max ->
      New = start_accept(Listen, Fun),
      socket_loop(Listen, New, Active, Fun, Max);

    _ ->
      socket_loop(Listen, false, Active, Fun, Max)
  end
.

start_accept(Listen, Fun) ->
  S = self(),
  spawn(fun() -> start_child(S, Listen, Fun) end)
.


%%启动子进程处理socket连接
start_child(Parent, Listen, Fun) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      Parent ! {istarted, self()},
      inet:setopts(Socket, [
        {packet, 4},
        binary,
        {nodelay, true},
        {active, true}
      ]),

      process_flag(trap_exit, true),
      case (catch Fun(Socket)) of
        {'EXIT', normal} -> true;
        {'EXIT', Why} ->
          io:format("Port process dies with exit :~p~n", [Why]),
          true;
        _ -> true
      end
  end
.












