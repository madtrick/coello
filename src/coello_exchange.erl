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
-module(coello_exchange).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([declare/3]).

-type exchange_type() :: direct | fanout | topic | headers.

-spec declare(Channel::pid(), Type::exchange_type(), ExchangeName::bitstring()) -> ok | blocked | closing.
declare(Channel, direct, ExchangeName) ->
  declare_exchange(Channel, <<"direct">>, ExchangeName);
declare(Channel, topic, ExchangeName) ->
  declare_exchange(Channel, <<"topic">>, ExchangeName);
declare(Channel, fanout, ExchangeName) ->
  declare_exchange(Channel, <<"fanout">>, ExchangeName).

-spec declare_exchange(Channel::pid, Type::bitstring(), ExchangeName::bitstring()) -> ok | blocked | closing.
declare_exchange(Channel, Type, ExchangeName) ->
  Method = #'exchange.declare'{ exchange = ExchangeName, type = Type, durable = false},

  case amqp_channel:call(Channel, Method) of
    #'exchange.declare_ok'{} ->
      ok;
    Other ->
      Other
  end.
