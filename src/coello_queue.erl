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
-module(coello_queue).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([declare/1, declare/2, bind/4, delete/2]).

-define(EMPTY_QUEUE_NAME, <<"">>).

-spec declare(Channel::pid()) -> {ok, QueueName::binary()} | blocked | closing.
declare(Channel)->
  declare(Channel, ?EMPTY_QUEUE_NAME).
  %Method = #'queue.declare'{exclusive = true},

  %case amqp_channel:call(Channel, Method) of
  %  #'queue.declare_ok'{queue = Queue} ->
  %    {ok, Queue};
  %  Other ->
  %    Other
  %end.

-spec declare(Channel::pid(), QueueName :: bitstring()) -> {ok, QueueName::binary()} | blocked | closing.
declare(Channel, QueueName)->
  Method = #'queue.declare'{exclusive = true, queue = QueueName},

  case amqp_channel:call(Channel, Method) of
    #'queue.declare_ok'{ queue = Queue} ->
      {ok, Queue};
    Other ->
      Other
  end.

-spec bind(Channel::pid(), QueueName::binary(), RoutingKey::bitstring(), Exchange::binary()) ->
  ok | blocked | closing.
bind(Channel, QueueName, RoutingKey, Exchange) ->
  Method = #'queue.bind'{queue = QueueName, routing_key = RoutingKey, exchange = Exchange},

  case amqp_channel:call(Channel, Method) of
    #'queue.bind_ok'{} ->
      ok;
    Other ->
      Other
  end.

-spec delete(Channel::pid(), QueueName::pid()) -> {ok, MsgCount::integer} | blocked | closing.
delete(Channel, QueueName) ->
  Method = #'queue.delete'{ queue = QueueName},

  case amqp_channel:call(Channel, Method) of
    #'queue.delete_ok'{ message_count = Count } ->
      {ok, Count};
    Other ->
      Other
  end.
