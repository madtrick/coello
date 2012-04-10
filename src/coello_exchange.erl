-module(coello_exchange).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([declare/3]).

-type exchange_type() :: direct | fanout | topic | headers.

-spec declare(Channel::pid(), Type::exchange_type(), ExchangeName::bitstring()) -> ok | blocked | closing.
declare(Channel, direct, ExchangeName) ->
  declare_exchange(Channel, <<"direct">>, ExchangeName);
declare(Channel, topic, ExchangeName) ->
  declare_exchange(Channel, <<"topic">>, ExchangeName);
declare(Channel, fanout, ExchangeName) ->
  declare_exchange(Channel, <<"fanout">>, ExchangeName).

-spec declare_exchange(Channel::pid, Type::bitstring(), ExchangeName::bitstring()) -> ok | blocked | closing.
declare_exchange(Channel, Type, ExchangeName) ->
  Method = #'exchange.declare'{ exchange = ExchangeName, type = Type, durable = false},

  case amqp_channel:call(Channel, Method) of
    #'exchange.declare_ok'{} ->
      ok;
    Other ->
      Other
  end.
