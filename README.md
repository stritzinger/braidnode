# braidnode

An OTP application to connect with a local braidnet instance and provide a transport for the Erlang Distribution traffic of the current node.

## Run without container
-----

    rebar3 shell

## Run as docker container

    rebar3 as container docker build
    docker run -ti local/braidnode
