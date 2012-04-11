-module(coello_channel_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  before_all(fun() ->
        meck:new([amqp_connection, amqp_channel]),
        meck:expect(amqp_connection, start, 1, pid),
        meck:expect(amqp_connection, open_channel, 1, {ok, channel})
    end),
  after_all(fun() ->
        meck:unload([amqp_connection, amqp_channel])
    end),
  describe("open", fun() ->
        it("should open a channel on the passed connection", fun() ->
              Connection = coello_connection:start(),
              coello_channel:open(Connection),

              assert_that(meck:called(amqp_connection, open_channel, [Connection]), is(true))
          end)
    end),
  describe("close", fun() ->
        it("should close a channel on the passed connection", fun() ->
              meck:expect(amqp_channel, close, 1, ok),

              Connection = coello_connection:start(),
              Channel = coello_channel:open(Connection),
              coello_channel:close(Channel),

              assert_that(meck:called(amqp_channel, close, [Channel]), is(true))
          end)
  end).
