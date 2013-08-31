%% @author Ron
%% @doc @todo Add description to widget.


-module(widget).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-compile(export_all).

start(Pid)->
	gs:start(),
	spawn_link(fun()->widget(Pid) end).


%%set方法区
get_state(Pid) ->	rpc(Pid,get_state).

set_title(Pid,Str)-> Pid ! {title,Str}.

set_handler(Pid,Fun) -> Pid ! {handler,Fun}.

set_prompt(Pid,Str) -> Pid ! {prompt,Str}.

set_state(Pid,Str) -> Pid ! {state,Str}.

insert_str(Pid,Str) -> Pid ! {insert,Str}.

update_state(Pid,N,X) ->  Pid ! {updateState,N,X}.

%% ====================================================================
%% Internal functions
%% ====================================================================
rpc(Pid,Q) ->
	Pid ! {self(),Q},
	receive
		{Pid,R} -> R
	end
.

widget(Pid) ->
	
	Size=[{width,500},{height,200}],
	Win=gs:window(gs:start(),
				  [{map,true},
				   {configure,true},
				   {title,"window"}|Size]),
	gs:frame(packer,Win,[{packer_x,[ {stretch,1,500}]},
						 {packer_y,[{stretch,10,120,100}]}]),
	
	gs:create(editor,editor,packer,
			  [{pack_x,1},
			   {pack_y,1},
			   {vscroll,right}
        ]),
	gs:create(entry, 
			  entry,
			  packer,
			  [{pack_x,1},
			  {pack_y,2},
			  {keypress,true}
        ]),
	gs:config(packer, Size),
	Prompt=">",
	State=nil,
	gs:config(entry, {insert,{0,Prompt}}),
	loop(Win,Pid,Prompt,State,fun parse/1)
.

loop(Win,Pid,Prompt,State,Parse) ->
		receive
			{From,get_state}->
				From ! {self(),State},
		    	loop(Win,Pid,Prompt,State,Parse);
		    
			{handler,Fun}->
				loop(Win,Pid,Prompt,State,Fun);
			
			{prompt,Str} ->
				gs:config(entry, {delete,{0,last}}),
				gs:config(entry, {insert,{0,Str}}),
				loop(Win,Pid,Str,State,Parse);
			{state,S} ->
				loop(Win,Pid,Prompt,S,Parse);
			{title,Str} ->
				gs:config(Win,[{title,Str}]),
				loop(Win,Pid,Str,State,Parse);

			{insert,Str}->
				gs:config(editor,{insert,{'end',Str}}),
				scroll_to_show_last_line(),
				loop(Win,Pid,Str,State,Parse);

			{updateState,N,X}->
				io:format("setelemtn N=~p X=~p Satte=~p~n",[N,X,State]),
				State1=setelement(N,State,X),
				loop(Win,Pid,Prompt,State1,Parse);
			

			{gs,_,destory,_,_} ->
				io:format("Destory ~n",[]),
				exit(windowDestoryed);
			{gs,entry,keypress,_,['Return'|_]}->
				Text=gs:read(entry,text),
				gs:config(entry, {delete,{0,last}}),
				gs:config(entry, {insert,{0,Prompt}}),
				try Parse(Text) of
					Term->
						Pid !{self(),State,Term}
				catch
					_:_ ->	self() ! {insert,"badinput"}
				end,
				loop(Win,Pid,Prompt,State,Parse);

			 {gs,_,configure,[],[W,H,_,_]} ->
				gs:config(packer,[{width,W},{heigth,H}]),
				loop(Win,Pid,Prompt,State,Parse);
				
			 {gs,entry,keypress,_,_} ->
				loop(Win,Pid,Prompt,State,Parse);

		      Any->
					io:format("Dioscard:~p~n",[Any]),
					loop(Win,Pid,Prompt,State,Parse)
		end
.


scroll_to_show_last_line()->
	Size=gs:read(editor, size),
	Height=gs:read(editor, height),
	CharHeight=gs:read(editor, char_height),
	TopRow=Size-Height/CharHeight,
	if TopRow >0 ->gs:config(editor, {vscrollpos,TopRow});
	   true -> gs:config(editor,{vscrollpos,0})
	end 
.

test() ->
	spawn(fun() -> test1() end) 
.

test1() ->
	W= widget:start(self()),
	set_title(W,"Test Window"),
	loop(W)

.
loop(W) ->
	receive
		{W,{str,Str}} ->
			Str1=Str++"\n",
			insert_str(W,Str1),
			loop(W)
	end
.
	
parse(Str) ->
	{str,Str}
.



