-module(seorsus_consumer_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(ChannelName, ConsumerSpec) ->
	supervisor:start_link(?MODULE, {ChannelName, ConsumerSpec}).

init({ChannelName, ConsumerSpec}) ->
	Spec = [{{If,Declare},
		{seorsus_consumer,start_link,[ChannelName,If,Declare]},
		permanent,
		2000,
		worker,
		[seorsus_consumer]} || {If,Declare} <- ConsumerSpec],
	{ok, {{one_for_one,1,2}, Spec}}.

