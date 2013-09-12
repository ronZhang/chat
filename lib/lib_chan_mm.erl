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


loop(Socket,Pid)->
    process_flag(trap_exit,true),
    loop1(Socket,Pid,false)
 .

loop1(Socket,Pid,Trace) ->

  receive
    {tcp,Socket,Bin} ->
      Term=binary_to_term(Bin),
      trace_it(Trace,{socketReceived,Term}),
      Pid ! {chan,self(),Term},
      loop1(Socket,Pid,Trace);
    {tcp_closed,Socket} ->
       trace_it(Trace,socketClosed),
       Pid ! {chan_closed,self()};
    {'EXIT',Pid,Why} ->
      trace_it(Trace,{controllingProcessExit,Why}),
      gen_tcp:close(Socket);
    {setController,Pid1}->
      trace_it(Trace,{changedController,Pid}),
      loop1(Socket,Pid1,Trace);

    {trace,Trace1} ->

      trace_it(Trace,{setTrace,Trace1}),
      loop1(Socket,Pid,Trace1);

    close-> trace_it(Trace,closedByClient),
      gen_tcp:close(Socket);

    {send,Term} ->

      trace_it(Trace,{sendingMessage,Term}),
      gen_tcp:send(Socket,term_to_binary(Term)),
      loop1(Socket,Pid,Trace);


     UUg-> io:format("lib_chan-mm:protocol error:~p~n",[UUg]),
     loop1(Socket,Pid,Trace)
  end
.

trace_it(false,_) -> void;
trace_it(true,{F,M})-> F(M).











































