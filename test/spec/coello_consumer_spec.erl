-module(coello_consumer_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("start/1", fun() ->
        it("should return the consumer's pid", fun()->
              Consumer = coello_consumer:start(fun() -> undefined end),

              assert_that(is_pid(Consumer), is(true))
        end),
      it("should invoke the callback for each received message", fun()->
            AmqpMsg = #amqp_msg{},
            Method  = #'basic.deliver'{},
            Message = {Method, AmqpMsg},
            Pid     = self(),
            Consumer = coello_consumer:start(fun(_) -> Pid ! on_message end ),
            Consumer ! Message,

            receive on_message -> ok end
      end)
  end).
