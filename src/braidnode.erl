-module(braidnode).


-export([register/2]).
-export([registered/0]).
-export([whereis/1]).
-export([send/2]).


register(Name, Pid) ->
    pg:join(braidnet, Name, Pid).

registered() ->
    pg:which_groups(braidnet).

whereis(Name) ->
    pg:get_members(braidnet, Name).

send(Name, Msg) ->
    Pids = pg:get_members(braidnet, Name),
    [P ! Msg || P <- Pids].
