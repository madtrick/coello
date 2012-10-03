-module(coello_basic_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  before_all(fun() ->
        meck:new([amqp_channel]),
        meck:expect(amqp_channel, cast, 2, ok),
        meck:expect(amqp_channel, cast, 3, ok)
    end),
  after_all(fun() ->
        meck:unload([amqp_channel])
    end),
  describe("publish/4", fun() ->
        it("should publish a binary message", fun()->
              Data = crypto:rand_bytes(30),
              Msg       = #amqp_msg{payload = Data },
              Method     = #'basic.publish'{ exchange = exchange, routing_key = routing_key},
              ok         = coello_basic:publish(channel, Data, exchange,routing_key),

              assert_that(meck:called(amqp_channel, cast, [channel, Method, Msg]), is(true))
          end),
        it("should publish a text message", fun()->
              Msg       = #amqp_msg{payload = <<"abc">> },
              Method     = #'basic.publish'{ exchange = exchange, routing_key =  routing_key},
              ok         = coello_basic:publish(channel, "abc", exchange, routing_key),

              assert_that(meck:called(amqp_channel, cast, [channel, Method, Msg]), is(true))
          end)
    end),
  describe("publish/5", fun() ->
    it("should publish a binary message", fun()->
              ReplyTo = <<"respondemeaqui">>,
              Data = crypto:rand_bytes(30),
              Msg       = #amqp_msg{payload = Data, props = #'P_basic'{reply_to = ReplyTo} },
              Method     = #'basic.publish'{ exchange = exchange, routing_key = routing_key},
              ok         = coello_basic:publish(channel, Data, exchange, routing_key, ReplyTo),

              assert_that(meck:called(amqp_channel, cast, [channel, Method, Msg]), is(true))
    end),
  it("should publish a text message", fun() ->
              Data = "abc",
              ReplyTo = <<"respondeaqui">>,
              Msg       = #amqp_msg{payload = <<"abc">>, props = #'P_basic'{reply_to = ReplyTo} },
              Method     = #'basic.publish'{ exchange = exchange, routing_key = routing_key},
              ok         = coello_basic:publish(channel, Data, exchange, routing_key, ReplyTo),

              assert_that(meck:called(amqp_channel, cast, [channel, Method, Msg]), is(true))
    end)
  end),
  describe("consume/3", fun() ->
        it("should consume messages and invoke the passed in callback", fun()->
              meck:expect(amqp_channel, subscribe, 3, #'basic.consume_ok'{consumer_tag = 1234}),

              QueueName = <<"queue">>,
              Method = #'basic.consume'{ queue = QueueName},
              Pid        = self(),
              {ConsumerPid, _} = coello_basic:consume(channel, QueueName, fun(_, _) -> Pid ! on_message end),
              ConsumerPid ! {#'basic.deliver'{}, #amqp_msg{}},

              assert_that(
                receive
                  on_message ->
                    true
                after 500 ->
                    false
                end, is(true)),
              assert_that(meck:called(amqp_channel, subscribe, [channel, Method, ConsumerPid]), is(true))
          end)
    end),
  describe("consume/4", fun() ->
        describe("when option 'no_ack' is present", fun() ->
              it("should set its value in the amqp method", fun() ->
                    meck:new(coello_consumer),
                    meck:expect(coello_consumer, start, 1, consumer),
                    meck:expect(amqp_channel, subscribe, 3, #'basic.consume_ok'{consumer_tag = 1234}),

                    Options = [{no_ack, true}],
                    QueueName = <<"queue">>,
                    Method = #'basic.consume'{ queue = QueueName, no_ack = true},
                    coello_basic:consume(channel, QueueName, fun(_, _) -> ok end, Options),

                    assert_that(meck:called(amqp_channel, subscribe, [channel, Method, '_']), is(true)),
                    meck:unload(coello_consumer)
                end)
          end)
    end),
  describe("cancel", fun() ->
        it("should cancel a consumer", fun() ->
              meck:new(coello_consumer),
              meck:expect(amqp_channel, call, 2, ok),
              meck:expect(coello_consumer, stop, 1, ok),

              Method = #'basic.cancel'{consumer_tag = consumer_tag},

              ok = coello_basic:cancel(channel, {consumer, consumer_tag}),

              assert_that(meck:called(amqp_channel, call, [channel, Method]), is(true)),
              assert_that(meck:called(coello_consumer, stop, [consumer]), is(true)),

              meck:unload(coello_consumer)
          end)
    end),
  describe("ack", fun() ->
        it("should ack a message", fun() ->
              DeliveryTag = tag,
              Multiple = 0,
              Method = #'basic.ack'{delivery_tag = DeliveryTag, multiple = Multiple},
              ok = coello_basic:ack(channel, DeliveryTag, Multiple),
              assert_that(meck:called(amqp_channel, cast, [channel, Method]), is(true))
          end)
    end),
  describe("reject", fun() ->
        it("should reject a message", fun() ->
              DeliveryTag = tag,
              Requeue = true,
              Method = #'basic.reject'{delivery_tag = DeliveryTag, requeue = Requeue},
              ok = coello_basic:reject(channel, DeliveryTag, Requeue),
              assert_that(meck:called(amqp_channel, cast, [channel, Method]), is(true))
          end)
    end).
