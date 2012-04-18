-module(coello_exchange_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("declare/3", fun() ->
        before_all(fun()->
              meck:new([amqp_channel]),
              meck:expect(amqp_channel, call, 2, #'exchange.declare_ok'{})
          end),
        after_all(fun() ->
              meck:unload([amqp_channel])
          end),

        it("should create a direct transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"direct">>, durable = false},
              ok         = coello_exchange:declare(channel, direct, Exchange),

              assert_that(meck:called(amqp_channel, call, [channel, Method]), is(true))
        end),
      it("should create a fanout transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"fanout">>, durable = false},
              ok         = coello_exchange:declare(channel, fanout, Exchange),

              assert_that(meck:called(amqp_channel, call, [channel, Method]), is(true))
      end),
      it("should create a topic transient exchange", fun()->
              Exchange   = <<"theExchange">>,
              Method     = #'exchange.declare'{exchange = Exchange, type = <<"topic">>, durable = false},
              ok         = coello_exchange:declare(channel, topic, Exchange),

              assert_that(meck:called(amqp_channel, call, [channel, Method]), is(true))
      end)
  end).
