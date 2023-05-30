-module(braidnode_connector).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    ping_nodes/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ping_nodes() ->
    gen_server:cast(?MODULE, ping_nodes).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(ping_nodes, State) ->
    Nodes = braidnode_epmd:get_connections(),
    lists:foreach(fun(Node) ->
        ?LOG_NOTICE("~n~p pings ~p: ~p~n", [node(), Node, net_adm:ping(Node)])
    end, Nodes),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
