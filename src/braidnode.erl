-module(braidnode).


-export([register/2]).
-export([registered/0]).
-export([whereis/1]).
-export([send/2]).

%% callbacks for the braidnode rebar3 plugin
-export([init/1,
         do/1,
         format_error/1]).

register(Name, Pid) ->
    pg:join(braidnet, Name, Pid).

registered() ->
    pg:which_groups(braidnet).

whereis(Name) ->
    pg:get_members(braidnet, Name).

send(Name, Msg) ->
    Pids = pg:get_members(braidnet, Name),
    [P ! Msg || P <- Pids].

%--- Callbacks for the braidnode rebar3 plugin ---------------------------------
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
