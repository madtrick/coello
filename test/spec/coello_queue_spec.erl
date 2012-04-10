-module(coello_queue_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  before_all(fun() ->
        meck:new([amqp_connection, amqp_channel]),
        meck:expect(amqp_connection, start, 1, connection),
        meck:expect(amqp_connection, open_channel, 1, {ok, channel})
    end),

  after_all(fun() ->
        meck:unload([amqp_connection, amqp_channel])
    end),

  describe("declare/1", fun() ->
        it("should create a exclusive, server named queue", fun()->
              meck:expect(amqp_channel, call, 2, #'queue.declare_ok'{ queue = <<"abc">>}),

              Params = #'queue.declare'{exclusive = true},
              Connection      = coello_connection:start(),
              Channel         = coello_channel:open(Connection),
              {ok, QueueName} = coello_queue:declare(Channel),

              assert_that(meck:called(amqp_channel, call, [Channel, Params]), is(true)),
              assert_that(QueueName, is(<<"abc">>))
        end)
  end),
describe("bind/4", fun() ->
      it("should bind the queue with the passed in routing key", fun()->
            meck:sequence(amqp_channel, call, 2,
              [#'queue.declare_ok'{}, #'queue.bind_ok'{}]),

            Connection      = coello_connection:start(),
            Channel         = coello_channel:open(Connection),
            {ok, QueueName} = coello_queue:declare(Channel),

            Exchange   = <<"exchange-A">>,
            RoutingKey = <<"pandecea">>,
            Params     = #'queue.bind'{queue = QueueName, exchange = Exchange, routing_key = RoutingKey },
            coello_queue:bind(Channel, QueueName, RoutingKey, Exchange),

            assert_that(meck:called(amqp_channel, call, [Channel, Params]), is(true))
        end)
  end).
