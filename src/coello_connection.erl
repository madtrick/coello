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

-export([start/0, start/1, close/1]).

-spec start() -> pid().
start() ->
  amqp_connection:start(#amqp_params_network{}).

-spec start(list({atom(), term()})) -> pid().
start(Options) ->
  AmqpParams = #amqp_params_network{},
  ConfiguredParams = configure_params(Options, AmqpParams),
  amqp_connection:start(ConfiguredParams).

-spec close(Connection::pid) -> ok.
close(Connection) ->
  amqp_connection:close(Connection).

%%%%%%%%%%%%%%%%%%%%
% Internals
%%%%%%%%%%%%%%%%%%%%
-spec configure_params(Options::list({atom(), term()}), Params::#amqp_params_network{}) -> #amqp_params_network{}.
configure_params([], Params) ->
  Params;
configure_params([{username, Username} | Options], Params) ->
  Result = setelement(#amqp_params_network.username, Params, Username),
  configure_params(Options, Result);
configure_params([{password, Password} | Options], Params) ->
  Result = setelement(#amqp_params_network.password, Params, Password),
  configure_params(Options, Result);
configure_params([{virtual_host, VirtualHost} | Options], Params) ->
  Result = setelement(#amqp_params_network.virtual_host, Params, VirtualHost),
  configure_params(Options, Result);
configure_params([{host, Host} | Options], Params) ->
  Result = setelement(#amqp_params_network.host, Params, Host),
  configure_params(Options, Result);
configure_params([{port, Port} | Options], Params) ->
  Result = setelement(#amqp_params_network.port, Params, Port),
  configure_params(Options, Result);
configure_params([{channel_max, ChannelMax} | Options], Params) ->
  Result = setelement(#amqp_params_network.channel_max, Params, ChannelMax),
  configure_params(Options, Result);
configure_params([{frame_max, FrameMax} | Options], Params) ->
  Result = setelement(#amqp_params_network.frame_max, Params, FrameMax),
  configure_params(Options, Result);
configure_params([{frame_max, FrameMax} | Options], Params) ->
  Result = setelement(#amqp_params_network.frame_max, Params, FrameMax),
  configure_params(Options, Result);
configure_params([{heartbeat, HeartBeat} | Options], Params) ->
  Result = setelement(#amqp_params_network.heartbeat, Params, HeartBeat),
  configure_params(Options, Result);
configure_params([{connection_timeout, ConnectionTimeout} | Options], Params) ->
  Result = setelement(#amqp_params_network.connection_timeout, Params, ConnectionTimeout),
  configure_params(Options, Result);
configure_params([{ssl_options, SSLOptions} | Options], Params) ->
  Result = setelement(#amqp_params_network.ssl_options, Params, SSLOptions),
  configure_params(Options, Result);
configure_params([{auth_mechanisms, AuthMechanisms} | Options], Params) ->
  Result = setelement(#amqp_params_network.auth_mechanisms, Params, AuthMechanisms),
  configure_params(Options, Result);
configure_params([{client_properties, ClientProperties} | Options], Params) ->
  Result = setelement(#amqp_params_network.client_properties, Params, ClientProperties),
  configure_params(Options, Result);
configure_params([{socket_options, SocketOptions} | Options], Params) ->
  Result = setelement(#amqp_params_network.socket_options, Params, SocketOptions),
  configure_params(Options, Result).
