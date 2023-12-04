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
    add_node_to_cluster/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_node_to_cluster() ->
    gen_server:cast(?MODULE, ?FUNCTION_NAME).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(add_node_to_cluster, State) ->
    {ok, Connections} = braidnode_epmd:register_with_braidnet(),
    ping_nodes(Connections),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

ping_nodes(Connections) ->
    lists:foreach(fun(Node) ->
        spawn(fun() -> ping_loop(Node) end)
    end, Connections),
    ?LOG_NOTICE("Names: ~p~n", [net_adm:names()]).

ping_loop(Node) ->
    Result = net_adm:ping(Node),
    ?LOG_NOTICE("~p pings ~p: ~p~n", [node(), Node, Result]),
    case Result of
        pong -> ok;
        pang ->
            timer:sleep(1000),
            ping_loop(Node)
    end.
