#!/bin/bash
# Build script for orb-core service
set -euo pipefail

SERVICE_NAME="orb-core"
VERSION="v1.0.0"
REGISTRY="${DOCKER_REGISTRY:-localhost:5000}"

echo "Building $SERVICE_NAME:$VERSION..."

docker build \
    -f Dockerfile.minimal \
    -t "${SERVICE_NAME}:latest" \
    -t "${SERVICE_NAME}:${VERSION}" \
    -t "${REGISTRY}/${SERVICE_NAME}:latest" \
    -t "${REGISTRY}/${SERVICE_NAME}:${VERSION}" \
    .

echo "✓ Built $SERVICE_NAME"
echo "  Images tagged:"
echo "    - ${SERVICE_NAME}:latest"
echo "    - ${SERVICE_NAME}:${VERSION}"
echo "    - ${REGISTRY}/${SERVICE_NAME}:latest"
echo "    - ${REGISTRY}/${SERVICE_NAME}:${VERSION}"
