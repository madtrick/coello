-module(coello_channel_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("open", fun() ->
        it("should open a channel on the passed connection", fun() ->
              meck:new(amqp_connection),
              meck:expect(amqp_connection, start, 1, pid),
              meck:expect(amqp_connection, open_channel, 1, {ok, channel}),

              Connection = coello_connection:start(),
              coello_channel:open(Connection),

              assert_that(meck:called(amqp_connection, open_channel, [Connection]), is(true)),
              meck:unload(amqp_connection)
          end)
    end).

