-module(braidnode_crypto).

-export([sign_fun/3]).

sign_fun(Msg, DigestType, _Opts) ->
    Result = braidnode_client:send_receive(sign, #{
        payload => base64:encode(erlang:term_to_binary(Msg)),
        hash_alg => DigestType,
        sign_alg => rsa
    }),
    case Result of
        Signature when is_binary(Signature) ->
            erlang:binary_to_term(base64:decode(Result));
        #{error := Reason} ->
            % The error will be catched by ssl and displayed as an alert
            error(Reason)
    end.
