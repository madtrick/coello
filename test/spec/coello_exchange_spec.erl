-module(coello_exchange_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("declare/3", fun() ->
        before_all(fun()->
              meck:new([amqp_connection, amqp_channel]),
              meck:expect(amqp_connection, start, 1, connection),
              meck:expect(amqp_connection, open_channel, 1, {ok, channel}),
              meck:expect(amqp_channel, call, 2, #'exchange.declare_ok'{})
          end),
        after_all(fun() ->
              meck:unload([amqp_channel, amqp_connection])
          end),

        it("should create a direct transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"direct">>, durable = false},
              Connection = coello_connection:start(),
              Channel    = coello_channel:open(Connection),
              ok         = coello_exchange:declare(Channel, direct, Exchange),

              assert_that(meck:called(amqp_channel, call, [Channel, Method]), is(true))
        end),
      it("should create a fanout transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"fanout">>, durable = false},
              Connection = coello_connection:start(),
              Channel    = coello_channel:open(Connection),
              ok         = coello_exchange:declare(Channel, fanout, Exchange),

              assert_that(meck:called(amqp_channel, call, [Channel, Method]), is(true))
      end),
      it("should create a topic transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"topic">>, durable = false},
              Connection = coello_connection:start(),
              Channel    = coello_channel:open(Connection),
              ok         = coello_exchange:declare(Channel, topic, Exchange),

              assert_that(meck:called(amqp_channel, call, [Channel, Method]), is(true))
      end)
  end).
