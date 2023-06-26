-module(braidnode_crypto).

-export([sign_fun/5]).

sign_fun(_Version, Msg, HashAlg, _Key, SignAlg) ->
    Result = braidnode_client:send_receive(sign, #{
        payload => base64:encode(erlang:term_to_binary(Msg)),
        hash_alg => HashAlg,
        sign_alg => SignAlg
    }),
    case Result of
        Signature when is_binary(Signature) ->
            erlang:binary_to_term(base64:decode(Result));
        #{error := Reason} ->
            % The error will be catched by ssl and displayed as an alert
            error(Reason)
    end.
