%% Copyright
-module(chat_client).
-author("Ron").


%%depend
-import(widget,
[get_state/1,
insert_str/2,
set_prompt/2,
set_state/2,
set_title/2,
set_handler/2,
update_state/3]).
%% API
-export([]).
-compile(export_all).

%%启动函数
start() ->
     connect("localhost",2223,"AsDT67aQ","general","joe")
  .


%%innerFunction

connect(Host, Port, HostPsw, Group, Nick) ->
           spawn( fun() -> handler(Host, Port, HostPsw, Group, Nick) end)
.

%%初始化
handler(Host, Port, HostPsw, Group, Nick) ->
  process_flag(trap_exit, true),
  Widget = widget:start(self()),
  set_title(Widget, Nick),
  set_state(Widget, Nick),
  set_prompt(Widget, [Nick, ">"]),
  set_handler(Widget, fun parse_command/1),
  start_connect(Host, Port, HostPsw),
  disconnect(Widget, Group, Nick)
.

%%断开连接
disconnect(Widget, Group, Nick) ->
  receive
    {connected, MM} ->
      insert_str(Widget, "connected to server\nsending data\n"),
%% todo lib_chan_mm:send(MM, {login, Group, Nick}),
      wait_login_response(Widget, MM);

    {Widget, destroyed} -> exit(died);
    {status, S} ->
      insert_str(Widget, to_str(S)),
      disconnect(Widget, Group, Nick);
    Other ->
      io:format("chat_client disconnected unexcepted:~p~n", [Other]),
      disconnect(Widget, Group, Nick)
  end

.
%% 登陆
wait_login_response(Widget, MM) ->
  receive
    {chan, MM, ack} -> active(Widget, MM);
    Other ->
      io:format("chat_client login unexpected", [Other, MM]),
      wait_login_response(Widget, MM)
  end
.


%%激活玩家状态j
active(Widget, MM)->
  receive
    {Widget,Nick,Str}->
       lib_chan_mm:send(MM,{replay,Nick,Str}),
       active(Widget, MM);
    {chan,MM,{msg,From,Pid,Str}}->
      insert_str(Widget,[From,"@",pid_to_list(Pid),"",Str,"\n"]),
      active(Widget, MM) ;
    {'EXIT',Widget,windowDestroyed} ->
      lib_chan_mm:close(MM);
    {close,MM} ->
      exit(serverDied);
    Other ->
      io:format("chat_client active unexpected:~P~n",[Other]),
      active(Widget,MM)
  end
.

start_connector(Host,Port,Pwd)->
  S=self(),
  spawn_link(fun()-> try_to_connect(S,Host,Port,Pwd) end)

.
try_to_connect(Parent,Host,Port,Pwd) ->
  case lib_chan:connect(Host,Port,chat,Pwd,[]) of
    {error,_Why} ->
      Parent ! {status,{cannot,connect,Host,Port}},
      sleep(2000),
      try_to_connect(Parent,Host,Port,Pwd) ;
    {ok,MM} ->
      lib_chan_mm:controller(MM,Parent),
      Parent ! {connected,MM},
      exit(connectorFinished)
  end
  .


sleep(T) ->
  receive
     after T -> true
  end
.

to_str(Term)->
  io:format("~p~n",[Term])
.

parse_command(Str) -> skip_to_gt(Str).

skip_to_get(">"++T)->T;
skip_to_get([_|T])->skip_to_get(T);
skip_to_get([])->exit("no >")  .
