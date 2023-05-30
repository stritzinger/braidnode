-module(braidnode_client).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([
    send_receive/1,
    send_receive/2
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    conn_pid,
    stream_ref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_receive(Method) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Method, undefined}).

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

handle_call({send_receive, Method, Params}, _From,
#state{conn_pid = ConnPid, stream_ref = StreamRef} = State) ->
    Resp = handle_send_receive(Method, Params, ConnPid, StreamRef),
    {reply, Resp, State};

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], Headers},
            #state{conn_pid = ConnPid, stream_ref = StreamRef} = S) ->
    ?LOG_NOTICE("Success in reaching the host!"),
    braidnode_epmd:ready(),
    timer:send_interval(10_000, ping), % TODO: adjust timing
    {noreply, #state{conn_pid = ConnPid, stream_ref = StreamRef}};

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
    ?LOG_ERROR("Unexpected ws msg: ~p",[Msg]),
    {noreply, S}.

handle_send_receive(Method, Params, ConnPid, StreamRef) ->
    RequestObj = jsonrpc_request_object(Method, Params),
    gun:ws_send(ConnPid, StreamRef, {binary, RequestObj}),
    receive
        {gun_ws, ConnPid, StreamRef, {binary, Frame}} ->
            ResponseObj = jiffy:decode(Frame, [return_maps]),
            maps:get(<<"result">>, ResponseObj)
    end.

-spec jsonrpc_request_object(binary(), term() | undefined) -> jiffy:json_value().
jsonrpc_request_object(Method, Params) ->
    Map1 = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => uuid:uuid_to_string(uuid:get_v4(), binary_standard),
        <<"method">> => Method
    },
    Map2 = case Params of
        undefined -> Map1;
        _ -> maps:put(<<"params">>, Params, Map1)
    end,
    jiffy:encode(Map2).
