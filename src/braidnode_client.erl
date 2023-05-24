-module(braidnode_client).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_link/0]).

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
    {ok, ContainerID} = inet:gethostname(),

    {ok, ConnPid} = gun:open(Domain, Port),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/braidnode", #{id => ContainerID}),
    {ok, #state{conn_pid = ConnPid, stream_ref = StreamRef}}.

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], Headers}, S) ->
    ?LOG_NOTICE("Success in reaching the host!"),
    {noreply, S};
handle_info({gun_response, ConnPid, _, _, Status, Headers}, S) ->
    exit({ws_upgrade_failed, Status, Headers});
handle_info({gun_error, ConnPid, StreamRef, Reason}, S) ->
   exit({ws_upgrade_failed, Reason});
handle_info(Msg, S) ->
    ?LOG_DEBUG("Unexpected ws msg: ~p",[Msg]),
    {noreply, S}.
