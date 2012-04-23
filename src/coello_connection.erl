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
-module(coello_connection).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start/0, close/1]).

-spec start() -> pid().
start() ->
  amqp_connection:start(#amqp_params_network{}).

-spec close(Connection::pid) -> ok.
close(Connection) ->
  amqp_connection:close(Connection).
