# Modified Erlang/OTP Docker Image

For security reasons braidnode containers cannot access their private key
used in Erlang distribution.

Instead, they rely on the local braidnet instance to hold the key and provide
signatures during TLS handshakes.

We are using a modified OTP build to handle SSL signatures with a custom function.

This dockerfile has the purpose of assembling a valid Erlang docker image that
then should be used to build braidnode docker images.
