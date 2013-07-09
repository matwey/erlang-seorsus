-module(seorsus_consumer).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {channel, interface, declare_fun, consumer_tag}).

-include_lib("amqp_client/include/amqp_client.hrl").

start_link(Channel, InterfaceProc, DeclareFun) when is_function(DeclareFun,1) ->
	gen_server:start_link(?MODULE, {Channel, InterfaceProc, DeclareFun}, []).

init({Channel, InterfaceProc, DeclareFun}) ->
	{ok, #state{channel=Channel,interface=InterfaceProc,declare_fun=DeclareFun}, 0}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(timeout, #state{channel=Channel,consumer_tag=undefined} = State) ->
	case catch do_declare(Channel, State#state.declare_fun) of
		{ok, Sub} ->
			#'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, self()),
			{noreply, State#state{consumer_tag = Tag}};
		{error, Error} ->
			{stop, Error, State};
		{_ErrClass, {error, Error}} ->
			{stop, Error, State}
	end;
handle_info({#'basic.deliver'{consumer_tag = Tag} = Deliver, #'amqp_msg'{props = Props, payload = Payload} = _Content}, #state{channel = Channel, consumer_tag = Tag, interface=If} = State) ->
	do_process_message(Payload, Props, Channel, Deliver, If),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, #state{channel = Channel, consumer_tag = Tag} = _State) ->
	case Tag of
		undefined -> ok;
		_ -> amqp_channel:cast(Channel, #'basic.cancel'{consumer_tag = Tag})
	end,
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% -type channel() :: pid() | atom()

% -spec do_declare(Channel :: channel(), DeclareFun :: fun( (channel()) -> {ok, #'basic.consume'{}} | {error, any()} )) -> {ok, #'basic.consume'{}} | {error, any()}
do_declare(Channel, DeclareFun) when is_function(DeclareFun,1) ->
	DeclareFun(Channel).

% -spec do_ack(Channel :: channel(), Deliver :: #'basic.deliver'{}) -> ok
do_ack(Channel, #'basic.deliver'{delivery_tag = Tag} = _Deliver) ->
	amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).
% -spec do_reject(Channel :: channel(), Deliver :: #'basic.deliver'{}) -> ok
do_reject(Channel, #'basic.deliver'{delivery_tag = Tag} = _Deliver) ->
	amqp_channel:cast(Channel, #'basic.reject'{delivery_tag = Tag, requeue = false}).

% -spec do_process_message(
%	Payload :: binary(),
%	Props :: #'P_basic'{},
%	Channel :: channel(),
%	Deliver :: #'basic.deliver'{},
%	If :: pid() | atom() ) ->
%       ok | {error, badarg} | {error, no_fun}
do_process_message(Payload, Props, Channel, Deliver, If) ->
	case catch gen_server:call(If, {amqp_msg, Payload, Props, Channel}) of
		ok -> do_ack(Channel, Deliver);
		{error, _} -> do_reject(Channel, Deliver);
		{_ErrClass, {error, _}} -> do_reject(Channel, Deliver)
	end.

