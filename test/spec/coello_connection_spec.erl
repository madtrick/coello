-module(coello_connection_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

spec() ->
  before_all(fun()->
        meck:new(amqp_connection),
        meck:expect(amqp_connection, start, 1, ok)
    end),
  after_all(fun() ->
        meck:unload(amqp_connection)
    end),
  describe("start/0", fun() ->
        it("should open a connection using the default parameters", fun() ->
              AmqpParams = #amqp_params_network{},
              coello_connection:start(),

              assert_that(meck:called(amqp_connection, start, [AmqpParams]), is(true))
          end)
    end),
  describe("start/1", fun() ->
        it("should configure connection with given parameters", fun() ->
              Options = [{host, "example"}, {port, 123}],
              AmqpParams = #amqp_params_network{ host = "example", port = 123},

              coello_connection:start(Options),

              assert_that(meck:called(amqp_connection, start, [AmqpParams]), is(true))
          end)
    end),
  describe("close/1", fun() ->
        it("should close the connection", fun()->
              meck:expect(amqp_connection, close, 1, ok),
              coello_connection:close(connection),

              assert_that(meck:called(amqp_connection, close, [connection]), is(true))
        end)
  end).
