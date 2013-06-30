-module(seorsus_channels_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(ConnectionName, ChannelSpec) ->
	supervisor:start_link(?MODULE, {ConnectionName, ChannelSpec}).

init({ConnectionName, ChannelSpec}) ->
	Spec = [[ {ChannelName,
		{seorsus_channel_sup, start_link, [ConnectionName, ChannelName, ConsumersSpec]},
		permanent,
		2000,
		supervisor,
		[seorsus_channel_sup]} || {ChannelName, ConsumersSpec} <- ChannelSpec]],
        {ok, {{one_for_one,1,2}, Spec}}.

