-module(coello_channel).

-export([open/1, close/1]).

-spec open(Connection::pid()) -> pid().
open(Connection) ->
  {ok, Channel} = amqp_connection:open_channel(Connection),
  Channel.

-spec close(Channel::pid()) -> ok | closing.
close(Channel) ->
  amqp_channel:close(Channel).
