-module(seorsus_utils_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

check_amqp_params_network_test() ->
	Proplist = [
		{non_field, nil},
		{username, guest},
		{password, guest},
		{virtual_host, <<"/">>},
		{host,localhost},
		{port,5672},
		{channel_max,0},
		{frame_max,0},
		{heartbeat,0},
		{ssl_options,none},
		{auth_mechanisms,none},
		{client_properties,[]}],

	Expect = #amqp_params_network{
		username = guest,
		password = guest,
		virtual_host = <<"/">>,
		host = localhost,
		port = 5672,
		channel_max = 0,
		frame_max = 0,
		heartbeat = 0,
		ssl_options = none,
		auth_mechanisms = none,
		client_properties = [] },

	?_assertEqual(Expect,seorsus_utils:amqp_params_network(Proplist)).

check_amqp_params_direct_test() ->
	Proplist = [
		{non_field, nil},
		{username, guest},
		{password, guest},
		{virtual_host, <<"/">>},
		{node, the_node},
		{client_properties,[]}],

	Expect = #amqp_params_direct{
		username = guest,
		password = guest,
		virtual_host = <<"/">>,
		node = the_node,
		client_properties = [] },

	?_assertEqual(Expect,seorsus_utils:amqp_params_direct(Proplist)).

