-module(braidnode_epmd).

-behaviour(gen_server).

% gen_server API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([ready/0]).
-export([get_connections/0]).

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

ready() ->
    gen_server:cast(?MODULE, ?FUNCTION_NAME).

get_connections() ->
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

% Unused
register_node(_Name, _Port) ->
    erlang:error(unexpected_call).

% 1st call when connecting to a node
address_please(Name, Host, AddressFamily) ->
    ?LOG_NOTICE("CALLING ~p~n", [?FUNCTION_NAME]),
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host, AddressFamily}).

% 2nd call when connecting to a node
port_please(Name, Host) ->
    ?LOG_NOTICE("CALLING ~p~n", [?FUNCTION_NAME]),
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host}).

port_please(_Name, _Host, _Timeout) ->
    erlang:error(unexpected_call).

names(Host) ->
    ?LOG_NOTICE("CALLING ~p~n", [?FUNCTION_NAME]),
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
    Method = atom_to_binary(M),
    Params = #{
        name => Name,
        host => Host
    },
    [<<"ok">>, Address] = braidnode_client:send_receive(Method, Params),

    {reply, {ok, erlang:list_to_tuple(Address)}, State};

handle_call({port_please = M, Name, Host}, _, State) when is_tuple(Host) -> % TODO
    Method = atom_to_binary(M),
    Params = #{
        name => list_to_binary(Name),
        host => erlang:tuple_to_list(Host)
    },
    case braidnode_client:send_receive(Method, Params) of
        [<<"ok">>, Port] ->
            ?LOG_NOTICE("Got ~p: ~p~n", [?FUNCTION_NAME, Port]),
            {reply, {port, Port, 6}, State};  % TODO
        [<<"ok">>, <<"noport">>] -> {reply, noport, State}
    end;

handle_call({names = M, Host}, _, State) ->
    Method = atom_to_binary(M),
    Params = #{
        host => Host
    },
    [<<"ok">>, Names] = braidnode_client:send_receive(Method, Params),
    {reply, {ok, Names}, State};

handle_call(get_connections, _, #state{connections = Connections} = State) ->
    {reply, Connections, State};

handle_call(Msg, _From, S) ->
    ?LOG_ERROR("Unexpected call: ~p",[Msg]),
    {reply, ok, S}.

handle_cast(ready, State) ->
    % --- register node
    Params = #{
        name => State#state.name,
        port => State#state.port
    },
    <<"ok">> = braidnode_client:send_receive(<<"register_node">>, Params),
    % ---
    [<<"ok">>, Connections] = braidnode_client:send_receive(<<"connections">>, [node()]),

    Nodes = [erlang:binary_to_atom(N) || N <- Connections],
    ?LOG_NOTICE("Received nodes: ~p~n", [Nodes]),

    braidnode_connector:ping_nodes(),

    {noreply, State#state{connections = Nodes}};

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast: ~p",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected info: ~p",[Msg]),
    {noreply, S}.
