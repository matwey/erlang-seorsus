-module(seorsus_utils).

-export([amqp_params_network/1]).
-export([amqp_params_direct/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

amqp_params_network(Proplist) ->
	lists:foldl(fun amqp_params_network_append/2, #amqp_params_network{}, Proplist).
amqp_params_direct(Proplist) ->
	lists:foldl(fun amqp_params_direct_append/2,  #amqp_params_direct{},  Proplist).

amqp_params_network_append({username,Value},Record) ->
	Record#amqp_params_network{username=Value};
amqp_params_network_append({password,Value},Record) ->
	Record#amqp_params_network{password=Value};
amqp_params_network_append({virtual_host,Value},Record) ->
	Record#amqp_params_network{virtual_host=Value};
amqp_params_network_append({host,Value},Record) ->
	Record#amqp_params_network{host=Value};
amqp_params_network_append({port,Value},Record) ->
	Record#amqp_params_network{port=Value};
amqp_params_network_append({channel_max,Value},Record) ->
	Record#amqp_params_network{channel_max=Value};
amqp_params_network_append({frame_max,Value},Record) ->
	Record#amqp_params_network{frame_max=Value};
amqp_params_network_append({heartbeat,Value},Record) ->
	Record#amqp_params_network{heartbeat=Value};
amqp_params_network_append({ssl_options,Value},Record) ->
	Record#amqp_params_network{ssl_options=Value};
amqp_params_network_append({auth_mechanisms,Value},Record) ->
	Record#amqp_params_network{auth_mechanisms=Value};
amqp_params_network_append({client_properties,Value},Record) ->
	Record#amqp_params_network{client_properties=Value};
amqp_params_network_append({_Key,_Value},Record) ->
	Record.

amqp_params_direct_append({username,Value},Record) ->
	Record#amqp_params_direct{username=Value};
amqp_params_direct_append({password,Value},Record) ->
	Record#amqp_params_direct{password=Value};
amqp_params_direct_append({virtual_host,Value},Record) ->
	Record#amqp_params_direct{virtual_host=Value};
amqp_params_direct_append({node,Value},Record) ->
	Record#amqp_params_direct{node=Value};
amqp_params_direct_append({client_properties,Value},Record) ->
	Record#amqp_params_direct{client_properties=Value};
amqp_params_direct_append({_Ket,_Value},Record) ->
	Record.
