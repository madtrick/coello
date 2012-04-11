-module(coello_connection).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start/0, close/1]).

-spec start() -> pid().
start() ->
  amqp_connection:start(#amqp_params_network{}).

-spec close(Connection::pid) -> ok.
close(Connection) ->
  amqp_connection:close(Connection).
