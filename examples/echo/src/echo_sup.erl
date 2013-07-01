-module(echo_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(AmqpParams) ->
        supervisor:start_link(?MODULE, {AmqpParams}).

init({AmqpParams}) ->
	ConnectionName = echo_amqp_connection,
	SingleConsumer = {echo_server, fun echo_server:declare_amqp/1},
	ConsumersSpec = [SingleConsumer],
	SingleChannel = {echo_amqp_channel, ConsumersSpec},
	ChannelsSpec = [SingleChannel],

	Seorsus = {seorsus_sup, {seorsus_sup, start_link, [ConnectionName, AmqpParams, ChannelsSpec]}, permanent, 2000, supervisor, [seorsus_sup]},
	Echo = {echo_server, {echo_server, start_link, []}, permanent, 2000, worker, [echo_server]},
        {ok, {{one_for_one,1,2}, [Seorsus, Echo]}}.

