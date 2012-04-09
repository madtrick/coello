-module(coello_connection).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start/0]).

-spec start() -> pid().
start() ->
  amqp_connection:start(#amqp_params_network{}).
