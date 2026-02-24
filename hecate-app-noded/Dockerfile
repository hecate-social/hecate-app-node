## Stage 1: Build
FROM erlang:27-alpine AS build

RUN apk add --no-cache git

WORKDIR /app

COPY rebar.config rebar.lock* ./
COPY config/ config/
COPY src/ src/
COPY apps/ apps/

RUN rebar3 as prod release

## Stage 2: Runtime
FROM alpine:3.22

RUN apk add --no-cache \
    libstdc++ \
    ncurses-libs \
    openssl

WORKDIR /app

COPY --from=build /app/_build/prod/rel/hecate_app_noded/ ./

ENV HOME=/root

ENTRYPOINT ["/app/bin/hecate_app_noded"]
CMD ["foreground"]
