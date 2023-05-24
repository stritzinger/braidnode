%%%-------------------------------------------------------------------
%% @doc gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(braidnode_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    braidnode_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
