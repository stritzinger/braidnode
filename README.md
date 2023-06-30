# braidnode

A dependency for deploying OTP applications onto
[braidnet](https://github.com/stritzinger/braidnet).

## Instructions
Add braidnode as a plugin to your global rebar3 configuration file: `~/.config/rebar3/rebar.config`.

For example:
```erlang
{plugins, [
    {braidnode, {git, "https://github.com/stritzinger/braidnode.git", {branch, "main"}}}
]}.
```

Then, generate a new app:

    $ rebar3 new braidnode_app <myapp>

The rebar3 project will be ready to assemble a docker image.

    rebar3 docker build
    docker tag local/your-app ....
    docker push ...

Make your dockerized release available on dockerhub so that braidnet will be able to pull it later.

## Build standalone braidnode application for braidnet
**This app can only be started as a container by braidnet.**

    rebar3 docker build
