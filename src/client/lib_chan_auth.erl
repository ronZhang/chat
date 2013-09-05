%% Copyright
-module(lib_chan_auth).
-author("Administrator").

%% API
-export([]).

-compile(export_all).



make_challenge()->
    random_string(25).
make_response(Challenge,Sceret)->
 lib_md5:string(Challenge,Sceret).

random_string(N)->
  random_seed(),
  random_string(N,[]).

random_string(0,D)->D;
random_string(N,D)->
  random_string(N-1,[random:uniform(26)-1+$a|D]).

random_seed()->
  {_,_,X}=erlang:now(),
  {H,M,S}=time(),
  H1=H*X rem 32767,
  M1=M*X  rem 32767,
  S1=S* X rem 32767,
  put(random_seed,{H1,M1,S1}).




