-module(coello_connection_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  describe("start/0", fun() ->
        it("should open a connection using the default parameters", fun() ->
              meck:new(amqp_connection),
              meck:expect(amqp_connection, start, 1, ok),
              AmqpParams = #amqp_params_network{},
              coello_connection:start(),

              assert_that(meck:called(amqp_connection, start, [AmqpParams]), is(true)),
              meck:unload(amqp_connection)
          end)
    end).

