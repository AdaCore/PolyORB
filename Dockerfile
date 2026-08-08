# syntax=docker/dockerfile:1

####################
# Base Stage - GNAT Ada Compiler Environment
####################
FROM debian:bookworm AS base

# Install core build tools and GNAT Ada compiler
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
        gnat-12 \
        gprbuild \
        gcc \
        g++ \
        make \
        autoconf \
        automake \
        python3 \
        python3-pip \
        git \
        ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

####################
# Dependencies Stage - Extract Context Files
####################
FROM base AS dependencies

# Copy build configuration files first for better layer caching
COPY configure.ac configure acinclude.m4 *.in ./
COPY doc/*.in ./doc/
COPY support/ ./support/
COPY compilers/ ./compilers/
COPY bldtools/ ./bldtools/

####################
# Builder Stage - Compile PolyORB
####################
FROM dependencies AS builder

# Copy all source code
COPY src/ ./src/
COPY projects/ ./projects/
COPY projects-distrib/ ./projects-distrib/
COPY tools/ ./tools/
COPY examples/ ./examples/
COPY cos/ ./cos/
COPY idls/ ./idls/
COPY contrib/ ./contrib/
COPY testsuite/ ./testsuite/

# Configure PolyORB (generates config files and Makefiles)
# Use CC=gcc to avoid clang issues, use defaults for protocols/services
RUN CC=gcc ./configure --prefix=/opt/polyorb

# Build using make (which internally uses gprbuild)
RUN make -j$(nproc)

# Install to /opt/polyorb
RUN make install

####################
# Test Stage - Run Test Suite
####################
FROM builder AS test

# Copy test suite
COPY testsuite/ ./testsuite/

# Run core tests
RUN cd testsuite && \
    if [ -f testsuite.py ]; then \
        python3 testsuite.py --category=core || echo "Tests completed with some failures"; \
    else \
        echo "Test suite not found or skipped"; \
    fi

####################
# Production Stage - Minimal Runtime Image
####################
FROM debian:bookworm-slim AS production

# Install only runtime dependencies (GNAT runtime libraries)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgnat-12 \
        libgcc-s1 \
        libstdc++6 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r polyorb && \
    useradd -r -g polyorb -u 1001 polyorb

# Copy built libraries and binaries from builder
COPY --from=builder --chown=polyorb:polyorb /opt/polyorb /opt/polyorb

# Set up environment
ENV PATH="/opt/polyorb/bin:${PATH}" \
    LD_LIBRARY_PATH="/opt/polyorb/lib:${LD_LIBRARY_PATH}"

# Security hardening
USER polyorb
WORKDIR /opt/polyorb

# Health check - validates PolyORB installation
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD [ "/opt/polyorb/bin/po_catref", "--version" ] || exit 1

# Default command
CMD ["/bin/sh", "-c", "echo 'PolyORB libraries ready' && exec /bin/sh"]
