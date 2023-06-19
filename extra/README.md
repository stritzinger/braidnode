# Hacked Erlang/OTP Docker Image

For security reasons braidnode containers cannot access their private key
used in Erlang distribution.

Instead, they rely on the local braidnet instance to hold the key and sign TLS handshake on its behalfs.

We are using a modifies OTP to redirect ssl signature to a custom function.

This docker file has the purpose of assembling a valid Erlang docker image that then should be used to build braidnode docker images.