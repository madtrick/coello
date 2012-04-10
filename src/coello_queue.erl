-module(coello_queue).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([declare/1, bind/4]).

-spec declare(Channel::pid()) -> {ok, QueueName::binary()} | blocked | closing.
declare(Channel)->
  Method = #'queue.declare'{exclusive = true},

  case amqp_channel:call(Channel, Method) of
    #'queue.declare_ok'{queue = Queue} ->
      {ok, Queue};
    Other ->
      Other
  end.

-spec bind(Channel::pid(), QueueName::binary(), RoutingKey::bitstring(), Exchange::binary()) ->
  ok | blocked | closing.
bind(Channel, QueueName, RoutingKey, Exchange) ->
  Method = #'queue.bind'{queue = QueueName, routing_key = RoutingKey, exchange = Exchange},

  case amqp_channel:call(Channel, Method) of
    #'queue.bind_ok'{} ->
      ok;
    Other ->
      Other
  end.