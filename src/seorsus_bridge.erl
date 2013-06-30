-module(seorsus_bridge).
-behaviour(supervisor_bridge).

-export([start_link/3,start_link_connection/2,start_link_channel/2]).
-export([init/1,terminate/2]).

-record(state, {start, terminate, pid}).

start_link_connection(Name, AmqpParams) ->
	start_link(Name, {amqp_connection, start, [AmqpParams]}, {amqp_connection, close}).

start_link_channel(Name, Connection) ->
	start_link(Name, {amqp_connection, open_channel, [Connection]}, {amqp_channel, close}).

start_link(Name, Start, Terminate) ->
	supervisor_bridge:start_link(?MODULE, {Name,Start,Terminate}).

init({Name, {M,F,A} = Start, Terminate}) ->
	{ok, Pid} = apply(M,F,A),
	register(Name, Pid),
	{ok, Pid, #state{start=Start,terminate=Terminate,pid=Pid}}.
	
terminate(_Reason, #state{terminate={M,F},pid=Pid} = _State) ->
	apply(M,F,[Pid]),
	ok.


