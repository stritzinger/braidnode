-module(braidnode_epmd).

% @doc This module replaces the built-in erl_epmd module.
% It communicates with the braidnet_epmd_server module of
% the local Braidnet instance to connect to other Braidnode nodes.

-behaviour(gen_server).

-export([
    register_with_braidnet/0
]).

% gen_server API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% EPMD API
-export([
    start_link/0,
    names/1,
    register_node/2,
    register_node/3,
    port_please/2,
    port_please/3,
    address_please/3,
    listen_port_please/2
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    name,
    port,
    connections
}).

register_with_braidnet() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

% 1st call at app startup.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 2nd call at app startup.
listen_port_please(Name, Host) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host}).

% 3rd call at app startup.
register_node(Name, Port, Driver) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Port, Driver}).

% Unused but exported for easier debugging.
register_node(_Name, _Port) ->
    erlang:error({unexpected_call, 'register_node/2'}).

% Called when connecting to a node
address_please(Name, Host, AddressFamily) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host, AddressFamily}).

% Unused but exported for easier debugging.
port_please(_Name, _Host) ->
    erlang:error({unexpected_call, 'port_please/2'}).

% Unused but exported for easier debugging.
port_please(_Name, _Host, _Timeout) ->
    erlang:error({unexpected_call, 'port_please/3'}).

% Called by net_adm:names/0.
names(Host) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host}).

init([]) ->
    {ok, #state{}}.

handle_call({listen_port_please, _Name, _Host}, _, State) ->
    Port = case os:getenv("BRD_EPMD_PORT") of
        false ->
            erlang:exit('BRD_EPMD_PORT_missing');
        PortString ->
            {P, []} = string:to_integer(PortString),
            P
    end,
    {reply, {ok, Port}, State};

handle_call({register_node, Name, Port, _Driver}, _, _) ->
    State1 = #state{name = Name, port = Port},
    {reply, {ok, 1}, State1};

handle_call({address_please = M, Name, Host, _AddressFamily}, _, State) ->
    % Although the official documentation states that the version of the
    % distribution protocol "has been 5 since Erlang/OTP R6",
    % erl_epmd:port_please/2 actually returns version 6.
    % So that's what we are using too.
    Version = 6,
    Method = atom_to_binary(M),
    Params = #{name => Name, host => Host },
    case braidnode_client:send_receive(Method, Params) of
        [<<"ok">>, Address, Port] ->
            {reply, {ok, erlang:list_to_tuple(Address), Port, Version}, State};
        [<<"error">>, <<"unknown">>] ->
            {reply, {error, unknown}, State};
        [<<"error">>, <<"nxdomain">>] ->
            {reply, {error, nxdomain}, State}
    end;

handle_call({names = M, Host}, _, State) ->
    Method = atom_to_binary(M),
    Params = #{host => Host},
    [<<"ok">>, Names] = braidnode_client:send_receive(Method, Params),
    {reply, {ok, Names}, State};

handle_call(register_with_braidnet, _, State) ->
    Method = <<"register_node">>,
    Params = #{name => State#state.name, port => State#state.port},
    [<<"ok">>, Connections] = braidnode_client:send_receive(Method, Params),
    Nodes = [erlang:binary_to_atom(N) || N <- Connections],
    {reply, {ok, Nodes}, State};

handle_call(Msg, _From, S) ->
    ?LOG_ERROR("Unexpected call: ~p",[Msg]),
    {reply, ok, S}.

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast: ~p",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected info: ~p",[Msg]),
    {noreply, S}.
