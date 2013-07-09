-module(echo_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([declare_amqp/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	{ok, nil}.

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_call({amqp_msg, Payload, Props, Channel}, _From, State) ->
	io:format("~w ~w ~w ~w ~n", [Payload, Props, Channel]),
	{reply, ok, State};
handle_call(_Msg, _From, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

declare_amqp(Channel) ->
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"echo_server">>, type = <<"direct">>}),
	Queue = #'queue.declare'{exclusive = true, auto_delete = true},
        #'queue.declare_ok'{queue = QueueName} = amqp_channel:call(Channel, Queue),
        #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue=QueueName, exchange = <<"echo_server">>, routing_key = <<"key">>}),	
	Sub = #'basic.consume'{queue = QueueName, no_ack = false, exclusive = true},
	{ok, Sub}.

