-module(braidnode_epmd).
-behaviour(gen_server).

% @doc This module replaces the built-in erl_epmd module.
% Instead of talking to EPMD, it communicates with the
% braidnet_epmd_server module of the local Braidnet instance
% for connecting to other Braidnode nodes.

-export([
    register_with_braidnet/0
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

% gen_server API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-type nodename() :: string() | atom(). % Name part of a node().
-type nodehost() :: string() | atom(). % Host part of a node().
-type oshost()   :: string(). % Operating system hostname.

-record(state, {
    name,
    port
}).

%-------------------------------------------------------------------------------
register_with_braidnet() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

%-------------------------------------------------------------------------------
% 1st call at node startup.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 2nd call at node startup.
% Returns the port this node listens on for incoming connections.
-spec listen_port_please(Name, Host) -> Result when
    Name   :: nodename(),
    Host   :: nodehost(),
    Result :: {ok, Port :: integer()}.
listen_port_please(Name, Host) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host}).

% 3rd call at node startup.
% This is meant to be used for registering the node with EPMD.
% But we just store the Name and Port values for later, and use them to
% register with Braidnet later, by calling register_with_braidnet/0
% when the WS connection has been set up.
-spec register_node(Name, Port, Driver) -> Result when
    Name   :: nodename(),
    Port   :: port(),
    Driver :: inet_tcp | inet6_tcp | inet | inet6,
    Result :: {ok, 1}.
register_node(Name, Port, Driver) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Port, Driver}).

% Called when connecting to a node.
% This asks (nicely!) for the IP of a node@host.
% By also including the port of the node in the return value,
% we save ourselves the trouble of also implementing port_please/2 or /3.
% ---
% The last item in the return value is the version of the distribution protocol.
% Note: The official documentation states that this version
% "has been 5 since Erlang/OTP R6", but the built-in erl_epmd module
% returns 6 as the version. So that's what we are using too.
-spec address_please(Name, Host, AddressFamily) -> Result | Error when
    Name          :: nodename(),
    Host          :: nodehost(),
    AddressFamily :: inet | inet6,
    Result        :: {ok, inet:ip_address(), port(), 6},
    Error         :: {error, term()}.
address_please(Name, Host, AddressFamily) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host, AddressFamily}).

% Called by net_adm:names/0 and :names/1.
% Returns the node names (and their ports) registered by Braidnet on a host.
% Note that this also includes nodes which this Braidnode did not connect to.
-spec names(Host) -> Result | Error when
    Host   :: oshost(),
    Result :: {ok, [{nodename(), port()}]},
    Error  :: {error, Reason :: atom()}.
names(Host) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host}).

%-------------------------------------------------------------------------------
% Unused but exported for easier debugging.
register_node(_Name, _Port) ->
    erlang:error({unexpected_call, 'register_node/2'}).

% Unused but exported for easier debugging.
port_please(_Name, _Host) ->
    erlang:error({unexpected_call, 'port_please/2'}).

% Unused but exported for easier debugging.
port_please(_Name, _Host, _Timeout) ->
    erlang:error({unexpected_call, 'port_please/3'}).

%-------------------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

handle_call({listen_port_please, _Name, _Host}, _, State) ->
    Port = case os:getenv("BRD_EPMD_PORT") of
        false ->
            erlang:exit({undefined, 'BRD_EPMD_PORT'});
        PortString ->
            {P, []} = string:to_integer(PortString),
            P
    end,
    {reply, {ok, Port}, State};

handle_call({register_node, Name, Port, _Driver}, _, _) ->
    State1 = #state{name = Name, port = Port},
    {reply, {ok, 1}, State1};

handle_call({address_please = M, Name, Host, _AddressFamily}, _, State) ->
    Version = 6,
    Method = atom_to_binary(M),
    Params = #{
        name => erlang:list_to_binary(Name),
        host => erlang:list_to_binary(Host)
    },
    case braidnode_client:send_receive(Method, Params) of
        #{<<"address">> :=  Address, <<"port">> := Port} ->
            {ok, IP} = inet:parse_address(Address),
            {reply, {ok, IP, Port, Version}, State};
        #{<<"error">> := Error} ->
            {reply, {error, binary_to_atom(Error)}, State}
    end;

handle_call({names = M, Host}, _, State) ->
    Method = atom_to_binary(M),
    Params = #{host => erlang:list_to_binary(Host), node => node()},
    NamesMap = braidnode_client:send_receive(Method, Params),
    Names = maps:fold(fun(Name, Port, Acc) ->
        [{binary_to_list(Name), Port} | Acc]
    end, [], NamesMap),
    {reply, {ok, Names}, State};

handle_call(register_with_braidnet, _, State) ->
    Method = <<"register_node">>,
    Params = #{name => State#state.name, port => State#state.port},
    #{<<"connections">> := C} = braidnode_client:send_receive(Method, Params),
    Nodes = [erlang:binary_to_atom(N) || N <- C],
    {reply, {ok, Nodes}, State};

handle_call(Msg, _From, S) ->
    logger:warning("Unexpected call: ~p",[Msg]),
    {reply, ok, S}.

handle_cast(Msg, S) ->
    logger:warning("Unexpected cast: ~p",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    logger:warning("Unexpected info: ~p",[Msg]),
    {noreply, S}.
