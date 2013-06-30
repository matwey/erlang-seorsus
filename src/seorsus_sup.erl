-module(seorsus_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(ConnectionName, AmqpParams, ChannelsSpec) ->
	supervisor:start_link(?MODULE, {ConnectionName, AmqpParams, ChannelsSpec}).

init({ConnectionName, AmqpParams, ChannelsSpec}) ->
	ConnectionSpec = {ConnectionName,
		{seorsus_bridge, start_link_connection, [ConnectionName, AmqpParams]},
		permanent,
		2000,
		worker,
		[seorsus_bridge]},
	ChannelSupSpec = {{ConnectionName, channels_sup},
		{seorsus_channels_sup, start_link, [ConnectionName, ChannelsSpec]},
		permanent,
		2000,
		supervisor,
		[seorsus_channels_sup]},
        {ok, {{rest_for_one,1,2}, [ConnectionSpec, ChannelSupSpec]}}.

