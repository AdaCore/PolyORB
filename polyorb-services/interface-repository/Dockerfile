# Multi-stage Dockerfile for Interface Repository (PolyORB Ada)
# Target: <150MB image, <3min build time

FROM ghcr.io/alire-project/gnat-x86_64-linux:13 AS builder

LABEL stage="builder"

RUN apt-get update && apt-get install -y \
    gprbuild gnat-13 make git libssl-dev pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY alire.toml alire.lock ./
RUN alr update && alr build --release || true

COPY *.gpr ./
COPY gpr/ ./gpr/ || true
COPY src/ ./src/

RUN gprbuild -P interface_repository.gpr \
    -XBUILD_MODE=release \
    -XLIBRARY_TYPE=static \
    -j$(nproc) -p \
    && strip obj/interface-repository

FROM debian:12-slim AS runtime

LABEL description="Interface Repository - CORBA interface repository"
LABEL version="1.0.0"

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgnat-13 libssl3 ca-certificates wget \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -r -u 65534 -s /sbin/nologin nobody

WORKDIR /app

COPY --from=builder /build/obj/interface-repository /app/
COPY --from=builder /build/obj/*.so* /app/ || true
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnat-13.so /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnarl-13.so /usr/lib/x86_64-linux-gnu/
COPY config/ ./config/ || true

RUN wget -q https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 -O /app/grpc_health_probe \
    && chmod +x /app/grpc_health_probe && chown nobody:nobody /app/grpc_health_probe

RUN chown -R nobody:nobody /app

USER nobody

EXPOSE 50066

HEALTHCHECK --interval=15s --timeout=5s --start-period=15s --retries=3 \
    CMD ["/app/grpc_health_probe", "-addr=:50066"]

ENTRYPOINT ["/app/interface-repository"]
CMD ["--port=50066"]
