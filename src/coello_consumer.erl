-module(coello_consumer).
-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

-record(state,{
    on_message :: fun()
  }).

-spec start(Callback::fun()) -> pid().
start(Callback) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Callback], []),
  Pid.

-spec init(Args::list()) -> {ok, #state{}}.
init([Callback]) ->
  {ok, #state{ on_message = Callback}}.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info({#'basic.deliver'{}, #amqp_msg{}}, State) ->
  (State#state.on_message)(22),
  {noreply, State}.

terminate(_, _) ->
  terminate.

code_change(_, State, _) ->
  {ok, State}.
