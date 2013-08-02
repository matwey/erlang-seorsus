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

amqp_channel_cast_publish_expect(X,Key,Payload) ->
	meck:new(amqp_channel,[]),
	Publish2 = #'basic.publish'{exchange=X,routing_key=Key},
	Msg2 = #amqp_msg{payload=Payload},
	meck:expect(amqp_channel, cast,  fun (channel, Publish, Msg) when Publish == Publish2, Msg == Msg2 -> ok; (_,_,_) -> error end).

amqp_channel_cast_publish_clean() ->
	meck:validate(amqp_channel),
	meck:unload(amqp_channel).

check_publish_by_exchange_test_() ->
	X = <<"exchangename">>,
	Key = <<"routing_key">>,
	Payload = <<"payload">>,

	{setup,
		fun () -> amqp_channel_cast_publish_expect(X,Key,Payload) end,
		fun (_) -> amqp_channel_cast_publish_clean() end,
		?_assertEqual(ok, seorsus_utils:publish(channel,X,Key,Payload))
	}.

check_publish_by_queue_test_() ->
	X = <<"">>,
	Q = <<"queue">>,
	Payload = <<"payload">>,

	{setup,
		fun () -> amqp_channel_cast_publish_expect(X,Q,Payload) end,
		fun (_) -> amqp_channel_cast_publish_clean() end,
		?_assertEqual(ok, seorsus_utils:publish(channel,Q,Payload))
	}.

check_publish_by_replyto_test_() ->
	X = <<"">>,
	Q = <<"queue">>,
	Payload = <<"payload">>,
	Props = #'P_basic'{reply_to=Q},

	{setup,
		fun () -> amqp_channel_cast_publish_expect(X,Q,Payload) end,
		fun (_) -> amqp_channel_cast_publish_clean() end,
		?_assertEqual(ok, seorsus_utils:publish(channel,Props,Payload))
	}.

