-module(coello_channel).

-export([open/1]).

-spec open(Connection::pid()) -> pid().
open(Connection) ->
  {ok, Channel} = amqp_connection:open_channel(Connection),
  Channel.
