%% Copyright
-module(lib_chan_tests).
-author("ron").

%% API
-export([]).




-author("ron").
-include_lib("eunit/include/eunit.hrl").
%% API
-import(lib_chan,[start_port_server/2
,start_server2/1
,start_server/0
,start_server/1
,start_erl_port_server/2
,start_port_instance/2
,start_server1/1
,get_service_definition/2
,really_start/3
,cast/2
,check_terms/1
,disconnect/1
,wait_close/1]).

-compile(export_all).


start_server_test()->
  start_server()
  .