FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-rpc-conduit"]
COPY --chown=builder:users . /work/hs-msgpack-rpc-conduit
RUN ["stack", "install", "msgpack-rpc-conduit"]

FROM scratch
COPY --from=build /home/builder/.local/ /
