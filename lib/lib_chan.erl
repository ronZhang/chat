%% Copyright
-module(lib_chan).
-author("Administrator").

%% API
-export([]).
-compile(export_all).


start_server() ->
  case os:getenv("HOME") of
      false->
        exit({ebadEnv,"HOME"});
      Home ->
        start_serve


  end.

start_server(ConfigFile)->
  io:format("lib_chan starting:~p~n",[ConfigFile]),
  case file:consult(ConfigFile) of
    {ok,ConfigData} ->
      io:format("ConfigData=~p~n",[ConfigData]),
      case check_terms(ConfigData) of
        []-> start_server(ConfigData);
        Errors ->
          exit({eDeaemonConfig,Errors})
      end;
    {error,Why} -> exit({eDaemonConfig,Why})
   end.


check_terms(ConfigData) ->
  L=map(fun check_term/1,ConfigData),
  [X||{error,X}<- L].

check_term({port,P}) when is_integer(P) -> ok;
check_term({service,_,password,_,mfa,_,_,_})  -> ok;
check_term(X)-> {error,{badTerm,X}}.


start_server1(ConfigData)->
  register(lib_chan,spawn(fun() ->start_server2(ConfigData) end) ).



start_server2(ConfigData) ->
  [Port]=[P|| {port,P} <- ConfigData],
   start_port_server(Port,ConfigData)
 .

 start_port_server(Port,ConfigData)->
   lib_chan_cs:start_raw_server(Port,
     fun(Socket) -> start_port_instance(Socket,ConfigData) end,
     100,
     4).


start_port_instance(Socket,ConfigData) ->
  S=self(),
  Controller=spawn_link(fun() -> start_erl_port_server(S,ConfigData) end ),
  lib_chan_mm:loop(Socket,Controller).

start_port_instance(MM,ConfigData) ->










