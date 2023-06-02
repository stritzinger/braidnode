-module(braidnode_log_h).
-export([log/2]).

log(LogEvent, #{formatter := {FModule, FConfig}}) ->
    IOList =  FModule:format(LogEvent, FConfig),
    braidnode_client:notify(log, #{<<"text">> => iolist_to_binary(IOList)}).
