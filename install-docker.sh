#!/bin/bash
# Docker Installation Helper for macOS (ARM64)
# Run this script: ./install-docker.sh

set -e

echo "============================================"
echo "Docker Installation Helper"
echo "============================================"
echo ""

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "Error: Homebrew is not installed"
    echo "Install from: https://brew.sh"
    exit 1
fi

echo "✓ Homebrew is installed"
echo ""

# Install Colima and Docker CLI
echo "Installing Colima and Docker CLI..."
echo "This will take 1-2 minutes..."
echo ""

brew install colima docker

echo ""
echo "✓ Colima and Docker installed"
echo ""

# Start Colima
echo "Starting Colima..."
echo "This creates a lightweight VM for running containers..."
echo ""

colima start --cpu 4 --memory 8

echo ""
echo "✓ Colima started"
echo ""

# Verify installation
echo "Verifying Docker installation..."
echo ""

docker --version
docker info

echo ""
echo "============================================"
echo "✓ Docker is ready!"
echo "============================================"
echo ""
echo "Next steps:"
echo "  1. Run: ./quickstart.sh"
echo "  2. This will build and deploy all 3 pilot services"
echo ""
echo "Colima commands:"
echo "  colima status   - Check if running"
echo "  colima stop     - Stop Colima"
echo "  colima start    - Start Colima"
echo "  colima delete   - Remove Colima VM"
echo ""
