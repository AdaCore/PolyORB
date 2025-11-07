#!/bin/bash

# Kubernetes Manifest Validation Script
# Validates YAML syntax and Kubernetes schema for all manifests

set -e

echo "===== Kubernetes Manifest Validation ====="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0
SUCCESS=0

# Function to validate YAML syntax
validate_yaml_syntax() {
    local file=$1
    if command -v yamllint &> /dev/null; then
        yamllint -d relaxed "$file" 2>&1
        return $?
    else
        # Basic YAML validation using Python
        python3 -c "import yaml; yaml.safe_load(open('$file'))" 2>&1
        return $?
    fi
}

# Function to validate Kubernetes manifest
validate_k8s_manifest() {
    local file=$1

    echo -n "Validating: $file ... "

    # Check YAML syntax
    if ! validate_yaml_syntax "$file" > /dev/null 2>&1; then
        echo -e "${RED}FAILED${NC} - Invalid YAML syntax"
        ((ERRORS++))
        return 1
    fi

    # Check if kubectl is available
    if command -v kubectl &> /dev/null; then
        if kubectl apply -f "$file" --dry-run=client > /dev/null 2>&1; then
            echo -e "${GREEN}OK${NC}"
            ((SUCCESS++))
            return 0
        else
            echo -e "${RED}FAILED${NC} - kubectl validation failed"
            kubectl apply -f "$file" --dry-run=client 2>&1 | head -5
            ((ERRORS++))
            return 1
        fi
    else
        echo -e "${YELLOW}SKIPPED${NC} - kubectl not available (YAML syntax OK)"
        ((WARNINGS++))
        return 0
    fi
}

# Validate namespaces
echo "=== Validating Namespaces ==="
for file in k8s/namespaces/*.yaml; do
    validate_k8s_manifest "$file"
done
echo ""

# Validate wxWidgets services
echo "=== Validating wxWidgets Services ==="
for service_dir in k8s/base/wxwidgets/*/; do
    service_name=$(basename "$service_dir")
    echo "-- Service: $service_name --"
    for file in "$service_dir"*.yaml; do
        validate_k8s_manifest "$file"
    done
    echo ""
done

# Validate PolyORB services
echo "=== Validating PolyORB Services ==="
for service_dir in k8s/base/polyorb/*/; do
    service_name=$(basename "$service_dir")
    echo "-- Service: $service_name --"
    for file in "$service_dir"*.yaml; do
        validate_k8s_manifest "$file"
    done
    echo ""
done

# Validate Helm charts
echo "=== Validating Helm Charts ==="
if command -v helm &> /dev/null; then
    echo -n "Validating: helm/wxwidgets ... "
    if helm lint ./helm/wxwidgets > /dev/null 2>&1; then
        echo -e "${GREEN}OK${NC}"
        ((SUCCESS++))
    else
        echo -e "${RED}FAILED${NC}"
        helm lint ./helm/wxwidgets 2>&1 | head -10
        ((ERRORS++))
    fi

    echo -n "Validating: helm/polyorb ... "
    if helm lint ./helm/polyorb > /dev/null 2>&1; then
        echo -e "${GREEN}OK${NC}"
        ((SUCCESS++))
    else
        echo -e "${RED}FAILED${NC}"
        helm lint ./helm/polyorb 2>&1 | head -10
        ((ERRORS++))
    fi
else
    echo -e "${YELLOW}SKIPPED${NC} - helm not available"
    ((WARNINGS++))
fi
echo ""

# Validate Kustomize overlays
echo "=== Validating Kustomize Overlays ==="
if command -v kustomize &> /dev/null; then
    for overlay in dev staging prod; do
        echo -n "Validating: k8s/overlays/$overlay ... "
        if kustomize build "k8s/overlays/$overlay" > /dev/null 2>&1; then
            echo -e "${GREEN}OK${NC}"
            ((SUCCESS++))
        else
            echo -e "${RED}FAILED${NC}"
            kustomize build "k8s/overlays/$overlay" 2>&1 | head -10
            ((ERRORS++))
        fi
    done
elif command -v kubectl &> /dev/null; then
    for overlay in dev staging prod; do
        echo -n "Validating: k8s/overlays/$overlay ... "
        if kubectl kustomize "k8s/overlays/$overlay" > /dev/null 2>&1; then
            echo -e "${GREEN}OK${NC}"
            ((SUCCESS++))
        else
            echo -e "${RED}FAILED${NC}"
            kubectl kustomize "k8s/overlays/$overlay" 2>&1 | head -10
            ((ERRORS++))
        fi
    done
else
    echo -e "${YELLOW}SKIPPED${NC} - kustomize/kubectl not available"
    ((WARNINGS++))
fi
echo ""

# Validate Istio configurations
echo "=== Validating Istio Configurations ==="
for file in istio/*.yaml; do
    validate_k8s_manifest "$file"
done
echo ""

# Summary
echo "===== Validation Summary ====="
echo -e "Success: ${GREEN}$SUCCESS${NC}"
echo -e "Warnings: ${YELLOW}$WARNINGS${NC}"
echo -e "Errors: ${RED}$ERRORS${NC}"
echo ""

if [ $ERRORS -gt 0 ]; then
    echo -e "${RED}Validation FAILED with $ERRORS errors${NC}"
    exit 1
else
    echo -e "${GREEN}Validation PASSED${NC}"
    if [ $WARNINGS -gt 0 ]; then
        echo -e "${YELLOW}Note: Some validations were skipped due to missing tools${NC}"
        echo "Install kubectl, helm, and kustomize for full validation"
    fi
    exit 0
fi
