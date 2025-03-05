-module(braidnode_crypto).

-export([sign_fun/3]).
-export([verify_fun/3]).

-include_lib("public_key/include/public_key.hrl").

-define(DEVICE_SERIAL_NUMBERS, "DEVICE_SERIAL_NUMBERS").
-define(ALWAYS_ALLOWED_NAMES, [<<"GRiSP2 CA">>]).

sign_fun(Msg, DigestType, Options) ->
    Result = braidnode_client:send_receive(sign, #{
        payload => base64:encode(erlang:term_to_binary(Msg)),
        hash_alg => DigestType,
        sign_options => maps:from_list(Options)
    }),
    case Result of
        Signature when is_binary(Signature) ->
            erlang:binary_to_term(base64:decode(Result));
        #{error := Reason} ->
            % The error will be catched by ssl and displayed as an alert
            error(Reason)
    end.

verify_fun(_OtpCert, {bad_cert, _} = Reason, _State) ->
    % io:format("Verify fun FAILED~n"),
    {fail, Reason};
verify_fun(_OtpCert, {extension, _E}, State) ->
    % io:format("Verify fun extension~n"),
    {unknown, State};
verify_fun(OtpCert, _Event, State) ->
    % io:format("Verify fun Event~n"),
    case os:getenv(?DEVICE_SERIAL_NUMBERS) of
        false ->
            io:format("NO DEVICE_SERIAL_NUMBERS~n"),
            {valid, State};
        DeviceSerials ->
            verify_subject(DeviceSerials, OtpCert, State)
    end.

verify_subject(DeviceSerials, OtpCert, State) ->
    BoardNames = gen_board_common_names(DeviceSerials),
    AllowedCommonNames = BoardNames ++ ?ALWAYS_ALLOWED_NAMES,
    % io:format("Allowed: ~p~n", [AllowedCommonNames]),
    CommonName = get_common_name(OtpCert),
    % io:format("CommonName: ~p~n", [CommonName]),
    case lists:member(CommonName, AllowedCommonNames) of
        true -> {valid, State};
        false ->
            io:format("Verification fun: common name not allowed ~p~n", [CommonName]),
            {fail, subject_not_allowed}
    end.

gen_board_common_names(DeviceSerials) ->
    [list_to_binary("GRiSP2 device " ++ N) || N <- string:split(DeviceSerials, ";", all)].

get_common_name(OtpCert) ->
    Sub = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    % io:format("Subject: ~p~n", [Sub]),
    {rdnSequence, [Attributes]} = Sub,
    [Name|_] = [
        Value
        ||
        {'AttributeTypeAndValue',{2,5,4,3},{utf8String,Value}} <- Attributes],
    Name.
