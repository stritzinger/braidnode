# braidnode

An OTP application to connect with a local braidnet instance and setup Erlang Distribution for the current Node.

## Run without container

    rebar3 shell

## Run as docker container with shell

You need to change the rebar3_docker dockerfile template to access the erlang console

    rebar3 docker build
    docker run -ti --network host local/braidnode
    docker run -d local/braidnode
    docker exec -ti interesting_edison /opt/braidnode/bin/braidnode remote_console
