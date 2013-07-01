-module(echo_app).
-behaviour(application).

-export([start/2,stop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start(_Type,_Args) ->
	{ok, Host} = application:get_env(echo, host),
	Params = #amqp_params_network{host = Host},
        echo_sup:start_link(Params).

stop(_State) ->
        ok.

