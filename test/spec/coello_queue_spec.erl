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
              {ok, QueueName} = coello_queue:declare(channel),

              assert_that(meck:called(amqp_channel, call, [channel, Params]), is(true)),
              assert_that(QueueName, is(<<"abc">>))
          end)
    end),
  describe("declare/2", fun() ->
        it("should create a exclusive queue named with the given name", fun() ->
              QueueName = <<"queuequeue">>,
              meck:expect(amqp_channel, call, 2, #'queue.declare_ok'{ queue = QueueName}),

              Params = #'queue.declare'{exclusive = true, queue = QueueName },
              {ok, CreatedQueue} = coello_queue:declare(channel, QueueName),

              assert_that(meck:called(amqp_channel, call, [channel, Params]), is(true)),
              assert_that(CreatedQueue, is(QueueName))
          end)
  end),
  describe("bind/4", fun() ->
        it("should bind the queue with the passed in routing key", fun()->
              meck:sequence(amqp_channel, call, 2,
                [#'queue.declare_ok'{}, #'queue.bind_ok'{}]),

              Exchange   = <<"exchange-A">>,
              RoutingKey = <<"pandecea">>,
              QueueName  = <<"cocacola">>,
              Params     = #'queue.bind'{queue = QueueName, exchange = Exchange, routing_key = RoutingKey },
              coello_queue:bind(channel, QueueName, RoutingKey, Exchange),

              assert_that(meck:called(amqp_channel, call, [channel, Params]), is(true))
          end)
    end),
  describe("delete/2", fun() ->
        it("should delete the queue unconditionally", fun()->
              meck:expect(amqp_channel, call, 2, #'queue.delete_ok'{}),

              QueueName = <<"acola">>,
              Method = #'queue.delete'{ queue = QueueName},
              coello_queue:delete(channel, QueueName),

              assert_that(meck:called(amqp_channel, call, [channel, Method]), is(true))
        end)
  end).
