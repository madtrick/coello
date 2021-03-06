-module(coello_consumer_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("start/1", fun() ->
        it("should return the consumer's pid", fun()->
              Consumer = coello_consumer:start(fun() -> undefined end),

              assert_that(is_pid(Consumer), is(true))
          end)
    end),
  it("should invoke the callback for each received message", fun()->
        AmqpMsg = #amqp_msg{},
        Method  = #'basic.deliver'{},
        Message = {Method, AmqpMsg},
        Pid     = self(),
        Consumer = coello_consumer:start(fun(_, _) -> Pid ! on_message end ),
        Consumer ! Message,

        receive on_message -> ok end
    end),
  it("should invoke the callback with the payload of the received message", fun() ->
        AmqpMsg = #amqp_msg{payload = "abc"},
        Method  = #'basic.deliver'{},
        Message = {Method, AmqpMsg},
        Pid     = self(),
        Consumer = coello_consumer:start(fun(Msg, _Replyto) -> Pid ! {on_message, Msg} end ),
        Consumer ! Message,

        assert_that(
          receive {on_message, "abc"} -> true
          after 500 -> false
          end,
          is(true))
    end),
  it("should pass the reply queue as second param if the callback has arity 2", fun() ->
        AmqpMsg = #amqp_msg{payload = "abc", props = #'P_basic'{reply_to = <<"replyqueue">>}},
        Method  = #'basic.deliver'{},
        Message = {Method, AmqpMsg},
        Pid     = self(),
        Consumer = coello_consumer:start(fun(Msg, ReplyQueue) -> Pid ! {on_message, Msg, ReplyQueue} end ),
        Consumer ! Message,

        assert_that(
          receive {on_message, "abc", <<"replyqueue">>} -> true
          after 500 -> false
          end,
          is(true))
    end),
  describe("stop/1", fun() ->
        before_all(fun() ->
              meck:new(amqp_channel),
              meck:expect(amqp_channel, call, 1, ok)
          end),
        after_all(fun() ->
              meck:unload(amqp_channel)
            end),
        it("should stop the consumer", fun() ->
              Consumer = coello_consumer:start(fun() -> undefined end),

              coello_consumer:stop(Consumer),

              assert_that(Consumer, isdead())
          end)
  end).
