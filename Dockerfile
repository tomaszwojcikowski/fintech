FROM erlang:21.0.9-alpine as builder

RUN apk add --no-cache --update tar curl git bash make libc-dev gcc g++ vim

RUN set -xe \
    && curl -fSL -o rebar3 "https://s3.amazonaws.com/rebar3-nightly/rebar3" \
    && chmod +x ./rebar3 \
    && ./rebar3 local install \
    && rm ./rebar3

ENV PATH "$PATH:/root/.cache/rebar3/bin"

ENV PATH "$PATH:/root/.cache/rebar3/bin"

WORKDIR /usr/src/app

# build and cache dependencies
COPY rebar.config rebar.lock /usr/src/app/
RUN rebar3 compile

# Copy our Erlang test application
COPY . /usr/src/app

# And build the release
RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

# Build stage 1
FROM alpine:3.8

RUN apk add --no-cache openssl-dev ncurses

WORKDIR /opt/fintech

ENV RELX_REPLACE_OS_VARS true
ENV NODE 127.0.0.1
ENV COOKIE fintech
ENV PEER_IP 127.0.0.1
ENV DISCOVERY_DOMAIN local

COPY --from=builder /opt/rel /opt/fintech

EXPOSE 8080 8080

ENTRYPOINT ["/opt/fintech/bin/fintech"]

CMD ["foreground"]