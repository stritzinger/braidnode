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
    % pending outgoing synchronous gen_server requests:
    pending = #{} :: #{RequestId :: binary() := From :: pid()},
    connected = false   :: true | false
}).

% API --------------------------------------------------------------------------

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
    RequestId = id(),
    Json = braidnode_jsonrpc:call(Method, Params, RequestId),
    ok = gun:ws_send(ConnPid, StreamRef, {binary, Json}),
    Pending = maps:put(RequestId, From, State#state.pending),
    {noreply, State#state{pending = Pending}};

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast({notify, Method, Params},
#state{conn_pid = ConnPid, stream_ref = StreamRef, connected = true} = State) ->
    Json = braidnode_jsonrpc:notification(Method, Params),
    ok = gun:ws_send(ConnPid, StreamRef, {binary, Json}),
    {noreply, State};
handle_cast(_, S) ->
    {noreply, S}.

handle_info({gun_ws, ConnPid, _, {binary, Frame}},
            #state{conn_pid = ConnPid, stream_ref = StreamRef} = State) ->
    NewS = case braidnode_jsonrpc:decode(Frame) of
        {call, Method, Params, ID}->
            Reply = handle_request(Method, Params, ID),
            ok = gun:ws_send(ConnPid, StreamRef, {binary, Reply}),
            State;
        {notification, Method, Params} ->
            handle_notification(Method, Params),
            State;
        {result, _Result, _ID} = Result ->
            handle_response(Result, State);
        {error, _Code, _Message, _Data, _ID} = Error ->
            handle_response(Error, State);
        {error, _Reason, EncodedReply} ->
            ok = gun:ws_send(ConnPid, StreamRef, {binary, EncodedReply}),
            State
    end,
    {noreply, NewS};

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers},
            #state{conn_pid = ConnPid, stream_ref = StreamRef} = S) ->
    ?LOG_NOTICE("Success in reaching the host!"),
    braidnode_connector:add_node_to_cluster(),
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

% INTERNAL ---------------------------------------------------------------------

handle_request(Method,  Params, ID) ->
    try call_method(Method, Params) of
        undefined ->  braidnode_jsonrpc:error(method_not_found, ID);
        Result -> braidnode_jsonrpc:result(Result, ID)
    catch Ex:Er:Stack ->
        ?LOG_ERROR("JsonRPC internal error ~p : ~p : ~p",[Ex, Er, Stack]),
        braidnode_jsonrpc:error(internal_error, ID)
    end.

handle_response({result, Result, ID}, #state{pending = Preqs} = S) ->
    #{ID := Caller} = Preqs,
    gen_server:reply(Caller, Result),
    S#state{pending = maps:remove(ID, Preqs)};
handle_response({error, _, _, _, ID} = Error,
                #state{pending = Preqs} = S) ->
    #{ID := Caller} = Preqs,
    gen_server:reply(Caller, Error),
    S#state{pending = maps:remove(ID, Preqs)}.

handle_notification(<<"shutdown">>, _Params) ->
    ?LOG_DEBUG("Reveived shutdown!"),
    init:stop();
handle_notification(Method, _) ->
    ?LOG_WARNING("Unhandled jsonrpc notification method ~p",[Method]).

call_method(<<"rpc">>, Params) ->
    ?LOG_DEBUG("Reveived RPC!"),
    execute_rpc(Params);
call_method(_, _) ->
    undefined.

execute_rpc(#{<<"m">> := M,<<"f">> := F, <<"a">> := A}) ->
    try
        Mod = binary_to_term(base64:decode(M), [safe]),
        Fun = binary_to_term(base64:decode(F), [safe]),
        Args = binary_to_term(base64:decode(A), [safe]),
        Pid = self(),
        spawn(fun() ->
            R =
            try
                erlang:apply(Mod, Fun, Args)
            catch Ex:Re:Stack ->
                pack_exception(Ex, Re, Stack)
            end,
            Pid ! {rpc_result, R}
        end),
        Result = receive
            {rpc_result, R} -> R
        after 60_000 ->
            rpc_timeout
        end,
        base64:encode(list_to_binary(io_lib:format("~p",[Result])))
    catch Ex:Re:Stack ->
        pack_exception(Ex, Re, Stack)
    end.

pack_exception(Ex, Re, Stack) ->
    #{exception => Ex,
      reason => Re,
      stack => list_to_binary(io_lib:format("~p", [Stack]))
    }.

id() -> uuid:uuid_to_string(uuid:get_v4(), binary_standard).
