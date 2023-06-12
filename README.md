# braidnode

An OTP application to connect with a local braidnet instance and setup Erlang Distribution for the current Node.

**This app can only be started as container by braidnet.**

The initial **WS connection is mandatory** and certificates for `inet6_tls_dist` must be mounted by `braidnet` at the expected location `/mnt/certs`.

## Build for braidnet

    rebar3 docker build


## Attach to container shell

    docker exec -ti <container name or id> /bin/ash


## Check logs

    docker logs <container name or id>


## Attach to Erlang console
TODO, can't resolve hostname

    docker exec -ti <container name or id> /opt/braidnode/bin/braidnode remote_console
