-module(coello_basic).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([publish/4, consume/3, cancel/1]).

-spec publish(Channel::pid(), Data::binary(), Exchange::bitstring(), RoutingKey::bitstring()) ->ok.
publish(Channel, Data, Exchange, RoutingKey) when is_binary(Data) ->
  publish_msg(Channel, #amqp_msg{payload = Data}, Exchange, RoutingKey);

publish(Channel, Data, Exchange, RoutingKey) when is_list(Data) ->
  publish_msg(Channel, #amqp_msg{payload = list_to_binary(Data)}, Exchange, RoutingKey).

-spec consume(Channel::pid(), QueueName::bitstring(), Callback::fun()) -> pid().
consume(Channel, QueueName, Callback) ->
  Consumer = coello_consumer:start(Callback),

  Method = #'basic.consume'{ queue = QueueName },
  ok = amqp_channel:subscribe(Channel, Method, Consumer),
  Consumer.

-spec cancel(Consumer::pid()) -> ok.
cancel(Consumer) ->
  coello_consumer:stop(Consumer).

-spec publish_msg(Channel::pid, Msg::#amqp_msg{}, Exchange::bitstring(), RoutingKey::bitstring()) ->
  ok.
publish_msg(Channel, Msg, Exchange, RoutingKey) ->
  Method = #'basic.publish'{ exchange = Exchange, routing_key = RoutingKey},
  amqp_channel:cast(Channel, Method, Msg).
