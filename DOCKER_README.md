# wxWidgets Microservices - Docker Build & Run Guide

## Overview

This repository contains Dockerfiles and configuration for 7 wxWidgets C++ microservices:

1. **Widget Core Service** (port 50051) - Main widget operations (~20K LoC)
2. **Render Manager Service** (port 50052) - Graphics rendering (~15K LoC)
3. **Event Manager Service** (port 50053) - Event handling (~10K LoC)
4. **Windows Adapter** (port 50054) - Windows-specific adapter (~12K LoC)
5. **macOS Adapter** (port 50055) - macOS-specific adapter (~14K LoC)
6. **Linux Adapter** (port 50056) - Linux-specific adapter (~8K LoC)
7. **XRC Service** (port 8080) - XML Resource handling (~6K LoC)

---

## Prerequisites

- **Docker** 20.10+ with BuildKit enabled
- **Docker Compose** 2.0+
- **Trivy** (optional, for security scanning)
- **4GB+ RAM** allocated to Docker

---

## Quick Start

### Build and Run All Services

```bash
# Build all services
./build.sh all

# Run with Docker Compose
docker compose up -d

# Check service health
docker compose ps

# View logs
docker compose logs -f widget-core
```

### Build Individual Service

```bash
# Build specific service
./build.sh widget-core

# Build with no cache
./build.sh render-manager --no-cache

# Build and push to registry
./build.sh event-manager --push
```

---

## Directory Structure

```
.
├── services/
│   ├── widget-core/
│   │   └── Dockerfile
│   ├── render-manager/
│   │   └── Dockerfile
│   ├── event-manager/
│   │   └── Dockerfile
│   ├── windows-adapter/
│   │   └── Dockerfile
│   ├── macos-adapter/
│   │   └── Dockerfile
│   ├── linux-adapter/
│   │   └── Dockerfile
│   └── xrc-service/
│       └── Dockerfile
├── docker-compose.yml
├── build.sh
└── DOCKER_README.md
```

---

## Dockerfile Architecture

All services use **multi-stage builds** for optimal image size:

### Stage 1: Builder
- Base: `ubuntu:24.04`
- Installs: build-essential, cmake, gRPC, Protobuf, wxWidgets
- Compiles service with C++17 standard
- Strips binary for size reduction

### Stage 2: Runtime
- Base: `ubuntu:24.04-slim`
- Runtime dependencies only (minimal)
- Non-root user (`nobody`)
- Health check integrated
- Target: **<100MB per image**

### Key Features

✅ **Multi-stage builds** - Separate build and runtime stages
✅ **Layer caching** - Dependencies cached for fast rebuilds
✅ **Security hardened** - Non-root user, minimal attack surface
✅ **Health checks** - gRPC health probes for all services
✅ **Small images** - <100MB per service
✅ **Fast builds** - <2min per service with caching

---

## Build Script Usage

The `build.sh` script provides comprehensive build management:

```bash
./build.sh [service-name|all] [options]
```

### Options

- `--no-cache` - Build without using Docker layer cache
- `--push` - Push images to registry after building
- `--scan` - Run Trivy security scan after building

### Examples

```bash
# Build all services
./build.sh all

# Build all with security scan
./build.sh all --scan

# Build single service without cache
./build.sh widget-core --no-cache

# Build and push to registry
./build.sh all --push

# Build, scan, and push
./build.sh all --scan --push
```

### Environment Variables

```bash
# Set custom registry
export DOCKER_REGISTRY=gcr.io/myproject
./build.sh all --push

# Set custom version tag
export VERSION=v1.2.3
./build.sh all

# Enable parallel builds (default: 4)
export BUILD_PARALLEL=8
./build.sh all
```

---

## Docker Compose

### Start Services

```bash
# Start all services in background
docker compose up -d

# Start specific service
docker compose up widget-core

# Build and start
docker compose up --build
```

### Stop Services

```bash
# Stop all services
docker compose down

# Stop and remove volumes
docker compose down -v
```

### View Status

```bash
# List running services
docker compose ps

# View logs
docker compose logs

# Follow logs for specific service
docker compose logs -f render-manager

# View last 100 lines
docker compose logs --tail=100
```

### Scale Services

```bash
# Scale render-manager to 3 replicas
docker compose up -d --scale render-manager=3
```

---

## Health Checks

All gRPC services use `grpc_health_probe` for health checks:

```bash
# Check health via Docker
docker exec widget-core /app/grpc_health_probe -addr=:50051

# Check health via grpcurl (if installed)
grpcurl -plaintext localhost:50051 grpc.health.v1.Health/Check
```

XRC Service (HTTP/REST) uses curl:

```bash
curl http://localhost:8080/health
```

---

## Security Scanning

### Install Trivy

```bash
# macOS
brew install aquasecurity/trivy/trivy

# Linux
wget https://github.com/aquasecurity/trivy/releases/download/v0.48.0/trivy_0.48.0_Linux-64bit.deb
sudo dpkg -i trivy_0.48.0_Linux-64bit.deb
```

### Scan Images

```bash
# Scan all services
./build.sh all --scan

# Scan specific service
trivy image widget-core:latest

# Scan for HIGH and CRITICAL only
trivy image --severity HIGH,CRITICAL widget-core:latest

# Generate JSON report
trivy image -f json -o scan-results.json widget-core:latest
```

### Acceptance Criteria

✅ **No CRITICAL vulnerabilities**
✅ **≤5 HIGH vulnerabilities** (with mitigation plan)
✅ **All dependencies up-to-date**

---

## Performance Targets

| Metric | Target | Status |
|--------|--------|--------|
| Image Size | <100MB | ✅ Optimized multi-stage builds |
| Build Time | <2min | ✅ Layer caching enabled |
| Startup Time | <10sec | ✅ Health checks validate |
| Memory Usage | 256-512MB | ✅ Resource limits in k8s manifests |

---

## Troubleshooting

### Build Failures

**Problem**: Build fails with "network timeout"

```bash
# Solution: Increase Docker timeout
export DOCKER_CLIENT_TIMEOUT=300
export COMPOSE_HTTP_TIMEOUT=300
./build.sh all
```

**Problem**: Out of disk space

```bash
# Solution: Clean up Docker resources
docker system prune -a --volumes
```

### Runtime Issues

**Problem**: Service won't start

```bash
# Check logs
docker compose logs widget-core

# Check health
docker inspect widget-core | grep -A 10 Health
```

**Problem**: gRPC health check fails

```bash
# Verify port is exposed
docker port widget-core 50051

# Test connectivity
grpcurl -plaintext localhost:50051 list
```

### Network Issues

**Problem**: Services can't communicate

```bash
# Check network
docker network inspect wxwidgets-network

# Verify DNS resolution
docker exec widget-core ping render-manager
```

---

## Development Workflow

### 1. Code Changes

Edit source code in your local environment.

### 2. Rebuild Service

```bash
# Rebuild specific service
./build.sh widget-core

# Or use Docker Compose
docker compose up --build widget-core
```

### 3. Test

```bash
# View logs
docker compose logs -f widget-core

# Run integration tests
docker compose exec widget-core /app/run-tests.sh
```

### 4. Iterate

```bash
# Quick rebuild without cache
./build.sh widget-core --no-cache
docker compose up -d widget-core
```

---

## Production Deployment

### Registry Configuration

```bash
# Login to registry
docker login gcr.io

# Set registry
export DOCKER_REGISTRY=gcr.io/myproject
export VERSION=v1.0.0

# Build and push
./build.sh all --push --scan
```

### Kubernetes Deployment

See [KUBERNETES_README.md](./KUBERNETES_README.md) for Kubernetes manifests and Helm charts.

---

## Best Practices

### Building

✅ **Use layer caching** - Build frequently-changing layers last
✅ **Multi-stage builds** - Separate build and runtime environments
✅ **Small base images** - Use -slim variants
✅ **Non-root users** - Run as nobody for security
✅ **Health checks** - Always include health check endpoints

### Security

✅ **Scan regularly** - Run Trivy on every build
✅ **Update dependencies** - Keep base images and packages current
✅ **Minimize attack surface** - Only install necessary packages
✅ **Read-only root fs** - Configure in Kubernetes manifests
✅ **No secrets in images** - Use environment variables or secrets management

### Performance

✅ **Cache dependencies** - Download/build deps in separate layer
✅ **Parallel builds** - Use BuildKit for better performance
✅ **Strip binaries** - Reduce binary size with strip command
✅ **Minimize layers** - Combine RUN commands where appropriate
✅ **Use .dockerignore** - Exclude unnecessary files from build context

---

## Metrics and Monitoring

### Prometheus Integration

All services expose metrics on `/metrics` endpoint:

```bash
# Check metrics
curl http://localhost:50051/metrics
```

### Grafana Dashboards

- Service health dashboard
- Resource utilization dashboard
- Request latency dashboard

---

## Contributing

### Adding a New Service

1. Create directory: `services/new-service/`
2. Add `Dockerfile` based on template
3. Update `docker-compose.yml`
4. Add service to `SERVICES` array in `build.sh`
5. Document in this README

### Dockerfile Template

See `services/widget-core/Dockerfile` for reference template.

---

## Support

- **Issues**: https://github.com/heathdorn00/wxWidgets/issues
- **Docs**: https://docs.wxwidgets.org/
- **Slack**: #wxwidgets-microservices

---

## License

MIT License - See [LICENSE](./LICENSE)

---

## Changelog

### v1.0.0 (2025-11-06)
- Initial release
- 7 microservices containerized
- Multi-stage Docker builds
- Docker Compose configuration
- Build automation scripts
- Security scanning integration
- Health checks implemented

---

**Built with ❤️ by RefactorTeam**
