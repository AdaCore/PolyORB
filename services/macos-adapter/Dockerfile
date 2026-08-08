# Multi-stage Dockerfile for macOS Adapter Service
# Target: <100MB image, <2min build time

FROM ubuntu:24.04 AS builder

LABEL stage="builder"

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    libgrpc++-dev \
    libprotobuf-dev \
    protobuf-compiler-grpc \
    libwxgtk3.2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY CMakeLists.txt ./
COPY cmake/ ./cmake/
RUN cmake -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build --target dependencies || true

COPY src/ ./src/
COPY include/ ./include/
COPY proto/ ./proto/

RUN cmake -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_STANDARD=17 \
    -DBUILD_SHARED_LIBS=OFF \
    && cmake --build build --config Release --parallel $(nproc) \
    && strip build/macos-adapter-service

FROM ubuntu:24.04-slim AS runtime

LABEL description="macOS Adapter Service - macOS-specific wxWidgets adapter"
LABEL version="1.0.0"

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgrpc++1.51 \
    libprotobuf32 \
    libwxgtk3.2-1 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -r -u 65534 -s /sbin/nologin nobody

WORKDIR /app

COPY --from=builder /build/build/macos-adapter-service /app/
COPY --from=builder /build/build/*.so* /app/ || true
COPY config/ ./config/ || true

ADD https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 /app/grpc_health_probe
RUN chmod +x /app/grpc_health_probe && chown nobody:nobody /app/grpc_health_probe

RUN chown -R nobody:nobody /app

USER nobody

EXPOSE 50055

HEALTHCHECK --interval=10s --timeout=3s --start-period=10s --retries=3 \
    CMD ["/app/grpc_health_probe", "-addr=:50055"]

ENTRYPOINT ["/app/macos-adapter-service"]
CMD ["--port=50055", "--workers=2"]
