# braidnode

A dependency for deploying OTP applications onto
[braidnet](https://github.com/stritzinger/braidnet).

This is also a rebar3 plugin with an application template to start developing [braid](https://github.com/stritzinger/braid) applications. Follow this readme to understand how to get started.

## Prerequisites

### Docker

To be able to produce braid applications you need to install [Docker](https://www.docker.com/). We use Docker to build images that will be used to create containers on the remote braidnet instances.

Currently we use [DockerHub](https://hub.docker.com/) to publicly host our images.
If you don't have an account, please create one on DockerHub so that you are able to push images on the cloud.

Make sure you are logged in the docker CLI

    docker login

If you need more guidance you can follow the [DockerHub Quickstart](https://docs.docker.com/docker-hub/quickstart/) guide to set yourself up.


## Plugin Instructions
Add braidnode as a plugin to your global rebar3 configuration file: `~/.config/rebar3/rebar.config`.

For example:
```erlang
{plugins, [
    {braidnode, {git, "https://github.com/stritzinger/braidnode.git", {branch, "main"}}}
]}.
```

Then, generate a new app:

    $ rebar3 new braidnode_app myapp


The rebar3 project will be ready to assemble a docker image.
You can check the docker guide on how to push an image:

    rebar3 docker build
    docker push local/myapp my-account/myapp

Make your dockerized release available on DockerHub so that braidnet will be able to pull it later. Make sure your repository is public so anyone, braidnet included, can pull it freely. We currently do not support private docker repositories.

## Build this project
This app, and any app depending on this app, can only be started as a container by braidnet. You can straight up clone this repository and build it like explained before.

## A note on the builder image specified in the rebar.config

We build images based on a special [Modified Erlang Dockerfile](https://github.com/stritzinger/braidnode/tree/main/extra)

Braidnode applications are using a modified Erlang/OTP build that allows
specifying a custom signing function for SSL handshakes.
This is used to delegate signing, as well as private key handling, to Braidnet.
