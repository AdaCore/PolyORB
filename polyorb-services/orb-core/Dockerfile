# Multi-stage Dockerfile for ORB Core Service (PolyORB Ada)
# Target: <150MB image, <3min build time
# Ada 2012 with GNAT FSF 13 compiler

# ============================================================================
# Stage 1: Builder - Build Ada service with GNAT and GPRBuild
# ============================================================================
FROM ghcr.io/alire-project/gnat-x86_64-linux:13 AS builder

# Metadata
LABEL maintainer="RefactorTeam"
LABEL description="Builder stage for ORB Core Service (Ada/PolyORB)"
LABEL stage="builder"

# Install build dependencies
RUN apt-get update && apt-get install -y \
    gprbuild \
    gnat-13 \
    make \
    git \
    libssl-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy Alire configuration first for caching
COPY alire.toml ./
COPY alire.lock ./

# Initialize Alire and fetch dependencies (cached layer)
RUN alr update && alr build --release || true

# Copy GPR project files
COPY *.gpr ./
COPY gpr/ ./gpr/ || true

# Copy source code
COPY src/ ./src/
COPY include/ ./include/ || true

# Build with GPRBuild in release mode
RUN gprbuild -P orb_core.gpr \
    -XBUILD_MODE=release \
    -XLIBRARY_TYPE=static \
    -j$(nproc) \
    -p \
    && strip obj/orb-core-service

# ============================================================================
# Stage 2: Runtime - Minimal Debian runtime with GNAT runtime libraries
# ============================================================================
FROM debian:12-slim AS runtime

# Metadata
LABEL maintainer="RefactorTeam"
LABEL description="ORB Core Service - CORBA ORB implementation in Ada"
LABEL version="1.0.0"
LABEL org.opencontainers.image.source="https://github.com/heathdorn00/PolyORB"
LABEL org.opencontainers.image.vendor="RefactorTeam"

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgnat-13 \
    libssl3 \
    ca-certificates \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -r -u 65534 -s /sbin/nologin nobody

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/obj/orb-core-service /app/
COPY --from=builder /build/obj/*.so* /app/ || true

# Copy GNAT runtime libraries
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnat-13.so /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgnarl-13.so /usr/lib/x86_64-linux-gnu/

# Copy configuration
COPY config/ ./config/ || true

# Download grpc_health_probe for health checks
RUN wget -q https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 -O /app/grpc_health_probe \
    && chmod +x /app/grpc_health_probe \
    && chown nobody:nobody /app/grpc_health_probe

# Set ownership
RUN chown -R nobody:nobody /app

# Switch to non-root user
USER nobody

# Expose CORBA port
EXPOSE 50060

# Health check
HEALTHCHECK --interval=15s --timeout=5s --start-period=15s --retries=3 \
    CMD ["/app/grpc_health_probe", "-addr=:50060"]

# Run the service
ENTRYPOINT ["/app/orb-core-service"]
CMD ["--port=50060", "--giop-version=1.2"]
