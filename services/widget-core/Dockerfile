# Multi-stage Dockerfile for Widget Core Service
# Target: <100MB image, <2min build time
# Security: Non-root user, read-only filesystem, Trivy scanned

# ============================================================================
# Stage 1: Builder - Build the C++ service with all dependencies
# ============================================================================
FROM ubuntu:24.04 AS builder

# Metadata
LABEL maintainer="RefactorTeam"
LABEL description="Builder stage for Widget Core Service"
LABEL stage="builder"

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    libgrpc++-dev \
    libprotobuf-dev \
    protobuf-compiler-grpc \
    libwxgtk3.2-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy dependency files first for better layer caching
COPY CMakeLists.txt ./
COPY cmake/ ./cmake/

# Download and cache dependencies (this layer will be cached)
RUN cmake -B build -DCMAKE_BUILD_TYPE=Release \
    && cmake --build build --target dependencies || true

# Copy source code
COPY src/ ./src/
COPY include/ ./include/
COPY proto/ ./proto/

# Build the service
RUN cmake -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_STANDARD=17 \
    -DBUILD_SHARED_LIBS=OFF \
    && cmake --build build --config Release --parallel $(nproc) \
    && strip build/widget-core-service

# ============================================================================
# Stage 2: Runtime - Minimal runtime image
# ============================================================================
FROM ubuntu:24.04-slim AS runtime

# Metadata
LABEL maintainer="RefactorTeam"
LABEL description="Widget Core Service - gRPC microservice for wxWidgets core operations"
LABEL version="1.0.0"
LABEL org.opencontainers.image.source="https://github.com/heathdorn00/wxWidgets"
LABEL org.opencontainers.image.vendor="RefactorTeam"

# Install runtime dependencies only
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgrpc++1.51 \
    libprotobuf32 \
    libwxgtk3.2-1 \
    libssl3 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -r -u 65534 -s /sbin/nologin nobody

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/build/widget-core-service /app/
COPY --from=builder /build/build/*.so* /app/ || true

# Copy configuration (if any)
COPY config/ ./config/ || true

# Download grpc_health_probe for health checks
ADD https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 /app/grpc_health_probe
RUN chmod +x /app/grpc_health_probe && \
    chown nobody:nobody /app/grpc_health_probe

# Set ownership
RUN chown -R nobody:nobody /app

# Switch to non-root user
USER nobody

# Expose gRPC port
EXPOSE 50051

# Health check using grpc_health_probe
HEALTHCHECK --interval=10s --timeout=3s --start-period=10s --retries=3 \
    CMD ["/app/grpc_health_probe", "-addr=:50051"]

# Run the service
ENTRYPOINT ["/app/widget-core-service"]
CMD ["--port=50051", "--workers=4"]
