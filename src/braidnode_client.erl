-module(braidnode_client).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([
    notify/1,
    notify/2,
    send_receive/1,
    send_receive/2
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    conn_pid,
    stream_ref,
    % pending synchronous gen_server requests:
    pending = #{} :: #{RequestId :: bitstring() := From :: pid()},
    connected = false   :: true | false
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Method) ->
    notify(Method, undefined).

notify(Method, Params) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Method, Params}).

send_receive(Method) ->
    send_receive(Method, undefined).

send_receive(Method, Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Method, Params}).

% gen_server callbacks ---------------------------------------------------------

init([]) ->
    {ok, Domain} = application:get_env(braidnet_domain),
    {ok, Port} = application:get_env(braidnet_port),
    ContainerID = os:getenv("CID"),
    {ok, ConnPid} = gun:open(Domain, Port),
    {ok, http} = gun:await_up(ConnPid, 5000),
    StreamRef = gun:ws_upgrade(ConnPid, "/braidnode", #{id => ContainerID}),
    % TODO: Need to do the node registration with Braidnet here,
    %       to ensure that Braidnode is ready by the time the user app starts.
    {ok, #state{conn_pid = ConnPid, stream_ref = StreamRef}}.

handle_call({send_receive, Method, Params}, From, State) ->
    #state{conn_pid = ConnPid, stream_ref = StreamRef} = State,
    {RequestId, RequestObj} = jsonrpc_object(request, Method, Params),
    ok = gun:ws_send(ConnPid, StreamRef, {binary, RequestObj}),
    Pending = maps:put(RequestId, From, State#state.pending),
    {noreply, State#state{pending = Pending}};

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast({notify, Method, Params},
#state{conn_pid = ConnPid, stream_ref = StreamRef, connected = true} = State) ->
    handle_notify(Method, Params, ConnPid, StreamRef),
    {noreply, State};
handle_cast(_, S) ->
    {noreply, S}.

handle_info({gun_ws, ConnPid, _, {binary, Frame}}, #state{conn_pid = ConnPid} = State) ->
    #{<<"result">> := Result,
      <<"id">> := Id
    } = jiffy:decode(Frame, [return_maps]),
    {From, Pending} = maps:take(Id, State#state.pending),
    gen_server:reply(From, Result),
    {noreply, State#state{pending = Pending}};

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers},
            #state{conn_pid = ConnPid, stream_ref = StreamRef} = S) ->
    ?LOG_NOTICE("Success in reaching the host!"),
    braidnode_connector:add_node_to_braidnet(),
    timer:send_interval(50_000, ping), % Default Cowboy timeout: 60_000
    {noreply, S#state{connected = true}};

handle_info({gun_response, ConnPid, _, _, Status, Headers},
            #state{conn_pid = ConnPid}) ->
    ?LOG_NOTICE("gun_response: ~p~n", [Status]),
    exit({ws_upgrade_failed, Status, Headers});

handle_info({gun_error, ConnPid, StreamRef, Reason},
            #state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    ?LOG_NOTICE("gun_error: ~p~n", [Reason]),
    exit({ws_upgrade_failed, Reason});

handle_info({gun_down, ConnPid, ws, closed, _},
            #state{conn_pid = ConnPid}) ->
    ?LOG_NOTICE("gun_down!~n"),
    exit({error, gun_down});

handle_info(ping, #state{conn_pid = ConnPid, stream_ref = StreamRef} = S) ->
    gun:ws_send(ConnPid, StreamRef, ping),
    {noreply, S};

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected ws msg: ~p~n",[Msg]),
    {noreply, S}.

handle_notify(Method, Params, ConnPid, StreamRef) ->
    RequestObj = jsonrpc_object(notification, Method, Params),
    gun:ws_send(ConnPid, StreamRef, {binary, RequestObj}).

-spec jsonrpc_object(request | notifination,
                             binary(),
                             term() | undefined) -> jiffy:json_value().
jsonrpc_object(Type, Method, Params) ->
    Map1 = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    },
    Map2 = case Params of
        undefined -> Map1;
        _ -> maps:put(<<"params">>, Params, Map1)
    end,
    ID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
     case Type of
        request ->
            {ID,  jiffy:encode(maps:put(<<"id">>, ID, Map2))};
        notification ->
            jiffy:encode(Map2)
    end.
