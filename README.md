# braidnode

An OTP application to connect with a local braidnet instance and setup Erlang Distribution for the current Node.


## Build for braidnet

    rebar3 docker build


## Attach to container shell

    docker exec -ti <container name or id> /bin/ash


## Check logs

    docker logs <container name or id>


## Attach to Erlang console
TODO, can't resolve hostname

    docker exec -ti <container name or id> /opt/braidnode/bin/braidnode remote_console
