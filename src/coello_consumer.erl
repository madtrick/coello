%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
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

handle_info(_ = #'basic.cancel_ok'{}, State) ->
  {noreply, State};

handle_info(#'basic.consume_ok'{ consumer_tag = Tag}, State) ->
 {noreply, State#state{consumer_tag = Tag}};

handle_info({#'basic.deliver'{}, #amqp_msg{ payload = Payload}}, State) ->
  (State#state.on_message)(Payload),
  {noreply, State}.

terminate(_, State) ->
  ok.
code_change(_, State, _) ->
  {ok, State}.
