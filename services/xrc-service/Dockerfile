# Multi-stage Dockerfile for XRC Service
# Target: <100MB image, <2min build time
# Note: XRC Service uses HTTP/REST instead of gRPC

FROM ubuntu:24.04 AS builder

LABEL stage="builder"

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    libwxgtk3.2-dev \
    libboost-all-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY CMakeLists.txt ./
COPY cmake/ ./cmake/
RUN cmake -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build --target dependencies || true

COPY src/ ./src/
COPY include/ ./include/

RUN cmake -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_STANDARD=17 \
    -DBUILD_SHARED_LIBS=OFF \
    && cmake --build build --config Release --parallel $(nproc) \
    && strip build/xrc-service

FROM ubuntu:24.04-slim AS runtime

LABEL description="XRC Service - XML Resource handling for wxWidgets"
LABEL version="1.0.0"

RUN apt-get update && apt-get install -y --no-install-recommends \
    libwxgtk3.2-1 \
    libboost-system1.83.0 \
    libboost-filesystem1.83.0 \
    libssl3 \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -r -u 65534 -s /sbin/nologin nobody

WORKDIR /app

COPY --from=builder /build/build/xrc-service /app/
COPY --from=builder /build/build/*.so* /app/ || true
COPY config/ ./config/ || true
COPY resources/ ./resources/ || true

RUN chown -R nobody:nobody /app

USER nobody

EXPOSE 8080

# HTTP health check for REST service
HEALTHCHECK --interval=10s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

ENTRYPOINT ["/app/xrc-service"]
CMD ["--port=8080", "--workers=2"]
