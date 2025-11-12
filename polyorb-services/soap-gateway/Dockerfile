# Multi-stage Dockerfile for SOAP Gateway (PolyORB Ada)
# Target: <150MB image, <3min build time
# Note: HTTP/SOAP service instead of pure CORBA

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

RUN gprbuild -P soap_gateway.gpr \
    -XBUILD_MODE=release \
    -XLIBRARY_TYPE=static \
    -j$(nproc) -p \
    && strip obj/soap-gateway

FROM debian:12-slim AS runtime

LABEL description="SOAP Gateway - SOAP to CORBA bridge for PolyORB"
LABEL version="1.0.0"

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgnat-13 libssl3 ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -r -u 65534 -s /sbin/nologin nobody

WORKDIR /app

COPY --from=builder /build/obj/soap-gateway /app/
COPY --from=builder /build/obj/*.so* /app/ || true
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnat-13.so /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnarl-13.so /usr/lib/x86_64-linux-gnu/
COPY config/ ./config/ || true

RUN chown -R nobody:nobody /app

USER nobody

EXPOSE 8081

# HTTP health check for SOAP service
HEALTHCHECK --interval=15s --timeout=5s --start-period=15s --retries=3 \
    CMD curl -f http://localhost:8081/health || exit 1

ENTRYPOINT ["/app/soap-gateway"]
CMD ["--port=8081"]
