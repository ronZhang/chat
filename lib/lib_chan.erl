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

start_erl_port_server(MM,ConfigData) ->
  receive
    {chan,MM,{startService,Mod,ArgC}}->
      case get_service_definition(Mod,ConfigData) of
        {yes,Pwd,MFA} ->
          case Pwd of
            none ->
              send(MM,ack),
              really_start(MM,ArgC,MFA);
            _ ->
              do_authentication(Pwd,MM,ArgC,MFA)
          end;
        no ->
          io:format("sending bad service~n"),
          send(MM,badService),
          close(MM)
      end;
    Any ->
      io:format("Erl port server got:~p~p~n",[MM,Any]),
      exit({protocolViolation,Any})
   end.


do_authentication(Pwd,MM,ArgC,MFA)->
  C=lib_chan_auth:make_challenge(),
  send(MM,{challenge,C}),
  receive
    {chan,MM,{response,R}}->
      case lib_chan_auth:is_response_correct(C,R,Pwd) of
        true ->
          send(MM,ack),
          really_start(MM,ArgC,MFA);
        false ->
          send(MM,authFail),
          close(MM)
      end
   end.

really_start(MM,ArgC,{Mod,Func,ArgS})->
  case (catch apply(Mod,Func,[MM,ArgC,ArgS])) of
    {'EXIT',normal}-> true;
    {'EXIT',Why} -> io:format("server error :~p~n",[why]);
    Why ->io:format("server error should die with exit was : ~p~n",[Why])
  end.

get_service_definition(Mod,[{service,Mod,password,Pwd,mfa,M,F,A}|_])  ->
  {yes,Pwd,{M,F,A}}   ;

get_service_definition(Name,[_|T])->
  get_service_definition(Name,T);
get_service_definition(_,[])-> no.


connect(Host,Port,Service,Secret,ArgC)->
    S=self(),
    MM=spawn(fun() ->connect(S,Host,Port)end),
    receive
      {MM,ok} ->
        case authenticate(MM,Service,Secret,ArgC) of
          ok -> {ok,MM};
          Error -> Error
        end;
      {MM,Error} -> Error
    end .







connect (Parent,Host,Port) ->
  case lib_chan_cs:start_raw_client(Host,Port,4) of
    {ok,Socket} ->
      Parent !{self(),ok},
      lib_chan_mm:loop(Socket,Parent);
    Error ->
      Parent !{self(),Error}
  end.



authenticate(MM,Service,Secret,Argc) ->
  send(MM,{startservice,Service,Argc}),
  receive
    {chan,MM,ack} -> ok;
    {chan,MM,{challenge,C}} ->
      R=lib_chan_auth:make_response(C,Secret),
      send(MM,{response,R}),
      receive
        {chan,MM,ack} ->
          ok;
        {chan,MM,authFail}->
          wait_close(MM),
          {error,authFail};
        Other ->{error,Other}
      end;
    {chan,MM,badService} ->
      wait_close(MM),
      {error,badService};
    Other ->
      {error,Other}
   end.



wait_close(MM)->
  receive
    {chan_closed,MM} ->
      true
    after 5000 ->
      io:format("error lib_chan~n"),
      true
    end.





disconnect(MM) -> close(MM).

rpc(MM,Q)->
  send(MM,Q),
  receive
    {chan,MM,Reply} ->
      Reply
  end.

cast(MM,Q) ->
  send(MM,Q).

























