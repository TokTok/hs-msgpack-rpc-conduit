# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-msgpack-rpc-conduit"]
COPY --chown=builder:users . /work/hs-msgpack-rpc-conduit
RUN ["stack", "build", "msgpack-rpc-conduit"]
