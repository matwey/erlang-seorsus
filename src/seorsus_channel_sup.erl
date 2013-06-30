-module(seorsus_channel_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(ConnectionName, ChannelName, ConsumerSpec) ->
	supervisor:start_link(?MODULE, {ConnectionName, ChannelName, ConsumerSpec}).

init({ConnectionName, ChannelName, ConsumerSpec}) ->
	ChannelSpec = {ChannelName,
		{seorsus_bridge, start_link_channel, [ChannelName, ConnectionName]},
		permanent,
		2000,
		worker,
		[seorsus_bridge]},
	ConsumerSupSpec = {{ChannelName, consumer_sup},
		{seorsus_consumer_sup, start_link, [ChannelName, ConsumerSpec]},
		permanent,
		2000,
		supervisor,
		[seorsus_consumer_sup]},
        {ok, {{rest_for_one,1,2}, [ChannelSpec, ConsumerSupSpec]}}.

