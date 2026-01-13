-module(braidnode_crypto).

-export([sign_fun/3]).
-export([verify_fun/3]).

-include_lib("public_key/include/public_key.hrl").

-define(DEVICE_IDENTIFIERS, "DEVICE_IDENTIFIERS").
-define(ALWAYS_ALLOWED_NAMES, [<<"GRiSP2 CA">>, <<"devices.seawater.local">>]).

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
    {fail, Reason};
verify_fun(_OtpCert, {extension, _E}, State) ->
    {unknown, State};
verify_fun(OtpCert, _Event, State) ->
    case os:getenv(?DEVICE_IDENTIFIERS) of
        false ->
            io:format("NO DEVICE_IDENTIFIERS~n"),
            {valid, State};
        DeviceIdentifiers ->
            verify_subject(DeviceIdentifiers, OtpCert, State)
    end.

%% Helper functions ------------------------------------------------------------

verify_subject(DeviceIdentifiers, OtpCert, State) ->
    DeviceNames = gen_device_cert_common_names(DeviceIdentifiers),
    AllowedCommonNames = DeviceNames ++ ?ALWAYS_ALLOWED_NAMES,
    CommonName = get_common_name(OtpCert),
    case lists:member(CommonName, AllowedCommonNames) of
        true -> {valid, State};
        false ->
            io:format("Verification fun: common name not allowed ~p~n", [CommonName]),
            {fail, subject_not_allowed}
    end.

gen_device_cert_common_names(DeviceIdentifiers) ->
    DeviceIdentifiersList = string:split(DeviceIdentifiers, ";", all),
    PlatformAndSerialList = [string:split(Identifier, "-", all)
                             || Identifier <- DeviceIdentifiersList],
    [iolist_to_binary([Platform, " device ", Serial])
     || [Platform, Serial] <- PlatformAndSerialList].

get_common_name(OtpCert) ->
    Sub = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    {rdnSequence, [Attributes]} = Sub,
    [Name|_] = [
        Value
        ||
        {'AttributeTypeAndValue',{2,5,4,3},{utf8String,Value}} <- Attributes],
    Name.
