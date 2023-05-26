-module(braidnode_client).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    conn_pid,
    stream_ref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks ---------------------------------------------------------

init([]) ->
    {ok, Domain} = application:get_env(domain),
    {ok, Port} = application:get_env(port),
    ContainerID = parse_container_id(),

    {ok, ConnPid} = gun:open(Domain, Port),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/braidnode", #{id => ContainerID}),
    {ok, #state{conn_pid = ConnPid, stream_ref = StreamRef}}.

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], Headers},
            #state{conn_pid = ConnPid, stream_ref = StreamRef} = S) ->
    ?LOG_NOTICE("Success in reaching the host!"),
    {noreply, S};
handle_info({gun_response, ConnPid, _, _, Status, Headers},
            #state{conn_pid = ConnPid}) ->
    exit({ws_upgrade_failed, Status, Headers});
handle_info({gun_error, ConnPid, StreamRef, Reason},
            #state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    exit({ws_upgrade_failed, Reason});
handle_info({gun_down, ConnPid, ws, closed, _},
            #state{conn_pid = ConnPid}) ->
    exit({error, gun_down});
handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected ws msg: ~p",[Msg]),
    {noreply, S}.


parse_container_id() ->
    % Very much non portable, but only way if the container is using the host network
    % In normal cases inet:gethostname() would hold the short container ID
    Line = os:cmd("cat /proc/self/mountinfo | "
                  "grep \"/docker/containers/\" | "
                  "head -1 | awk '{print $4}' "),
    [_, "docker", "containers", ID | _] = string:split(string:trim(Line), "/", all),
    ID.