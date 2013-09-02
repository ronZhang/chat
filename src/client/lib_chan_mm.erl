%% Copyright
-module(lib_chan_mm).
-author("Ron").

%% API
-export([]).

-compile(export_all).



%% 发送消息
send(Pid,Term)->
  Pid ! {send,Term}.

close(Pid) -> Pid ! close .
controller(Pid,Pid1) -> Pid ! {setController,Pid1}.
set_trace(Pid,X) -> Pid ! {trace,X}.
%%
trace_with_tag(Pid,Tag) -> set_trace(Pid,{true,
fun(Msg) -> io:format("MM:~p~p~n",[Tag,Msg]) end
}).































