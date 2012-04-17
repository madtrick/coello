-module(coello_consumer).
-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

-record(state,{
    on_message :: fun(),
      consumer_tag
  }).

-spec start(Callback::fun()) -> pid().
start(Callback) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Callback], []),
  Pid.

-spec stop(Consumer::pid) -> ok.
stop(Consumer) ->
  gen_server:call(Consumer, stop).

-spec init(Args::list()) -> {ok, #state{}}.
init([Callback]) ->
  {ok, #state{ on_message = Callback}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(#'basic.consume_ok'{ consumer_tag = Tag}, State) ->
 {noreply, State#state{consumer_tag = Tag}};

handle_info({#'basic.deliver'{}, #amqp_msg{ payload = Payload}}, State) ->
  (State#state.on_message)(Payload),
  {noreply, State}.

terminate(_, State) ->
  amqp_channel:call(#'basic.cancel'{consumer_tag = State#state.consumer_tag}).

code_change(_, State, _) ->
  {ok, State}.
