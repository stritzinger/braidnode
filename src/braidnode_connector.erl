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
    add_node_to_braidnet/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_node_to_braidnet() ->
    gen_server:cast(?MODULE, ?FUNCTION_NAME).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(add_node_to_braidnet, State) ->
    {ok, Connections} = braidnode_epmd:register_with_braidnet(),
    ping_nodes(Connections),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

ping_nodes(Connections) ->
    lists:foreach(fun(Node) ->
        ?LOG_NOTICE("~p pings ~p: ~p~n", [node(), Node, net_adm:ping(Node)])
    end, Connections).
