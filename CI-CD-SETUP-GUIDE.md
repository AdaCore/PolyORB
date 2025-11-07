# CI/CD Setup Guide

**Version**: 1.0.0
**Date**: 2025-11-06
**Status**: ✅ CONFIGURED AND READY

## Overview

This project now has a complete CI/CD pipeline configured with GitHub Actions. The pipeline includes:

- **Build & Test**: Automated building, testing, and Docker image creation
- **Security Scanning**: Comprehensive security checks on every commit
- **Deployment**: Automated deployment to dev/staging/prod environments
- **Performance**: Automated performance benchmarking

## Pipeline Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    GitHub Push/PR                            │
└───────────────────┬─────────────────────────────────────────┘
                    │
        ┌───────────┴───────────┐
        │                       │
        ▼                       ▼
┌──────────────┐       ┌──────────────┐
│ Build & Test │       │   Security   │
│   Workflow   │       │   Scanning   │
└──────┬───────┘       └──────┬───────┘
       │                      │
       │ ✓ All checks pass    │
       │                      │
       └──────────┬───────────┘
                  │
                  ▼
         ┌────────────────┐
         │   Docker Build │
         │   & Push       │
         └────────┬───────┘
                  │
                  ▼
         ┌────────────────┐
         │   Deploy       │
         │   (dev/staging │
         │   /prod)       │
         └────────────────┘
```

## Workflows

### 1. Build & Test (`.github/workflows/build-test.yml`)

**Triggers**:
- Push to `main`, `develop`, `feature/**`, `refactor/**`
- Pull requests to `main`, `develop`
- Manual dispatch

**Jobs**:
1. **Build C++ Services** - Compiles widget-core, xrc-service
2. **Build Ada Services** - Compiles orb-core with GPRbuild
3. **Test Suite** - Runs Jest, mutation tests, coverage
4. **Build Docker Images** - Creates and scans container images
5. **Integration Tests** - Tests services in Kind cluster
6. **Build Summary** - Reports results on PRs
7. **Performance Benchmarks** - Runs k6 load tests (main only)

**Outputs**:
- Test results (XML/JSON)
- Coverage reports
- Docker images pushed to GHCR
- Security scan results (SARIF)

### 2. Security Scanning (`.github/workflows/security.yml`)

**Triggers**:
- Push to `main`, `develop`, `feature/**`
- Pull requests
- Daily at 2 AM UTC (scheduled)

**Jobs**:
1. **Secret Scanning** - TruffleHog, GitLeaks, detect-secrets
2. **SAST** - cppcheck, clang-tidy, Semgrep
3. **Container Scanning** - Trivy, Grype, Dockle
4. **SBOM Generation** - Creates software bill of materials
5. **Dependency Scanning** - CVE detection
6. **License Compliance** - FOSSA scanning
7. **Policy Validation** - OPA/Conftest checks
8. **OpenSSF Scorecard** - Security best practices
9. **Security Summary** - Consolidated report

**Outputs**:
- SARIF files uploaded to GitHub Security tab
- Security report on PRs
- SBOM artifacts (CycloneDX format)

### 3. Deployment (`.github/workflows/deploy.yml`)

**Triggers**:
- Manual dispatch (workflow_dispatch) for any environment
- Automatic on push to `main` (deploys to dev)

**Environments**:
- **dev**: Automatic deployment, fast feedback
- **staging**: Blue-green deployment, requires approval
- **prod**: Canary deployment with gradual rollout, requires approval

**Jobs**:
1. **Validate Deployment** - Validates K8s manifests
2. **Deploy to Dev** - Automatic deployment
3. **Deploy to Staging** - Blue-green with approval
4. **Deploy to Production** - Canary with monitoring
5. **Rollback** - Emergency rollback capability

## Setup Instructions

### Step 1: Configure GitHub Repository

#### 1.1 Push Code to GitHub

```bash
# Add remote
git remote add origin https://github.com/YOUR_USERNAME/code_architect.git

# Initial commit
git add .
git commit -m "Initial commit: CI/CD pipeline configured"

# Push to GitHub
git push -u origin main
```

#### 1.2 Enable GitHub Actions

1. Go to your repository on GitHub
2. Click **Settings** → **Actions** → **General**
3. Under "Actions permissions", select **Allow all actions and reusable workflows**
4. Click **Save**

#### 1.3 Configure GitHub Container Registry

```bash
# Enable GHCR for your repository
# Settings → Actions → General → Workflow permissions
# Select: Read and write permissions
# Save
```

### Step 2: Configure Secrets

Go to **Settings** → **Secrets and variables** → **Actions** and add:

#### Required Secrets

| Secret Name | Description | How to Get |
|------------|-------------|------------|
| `GITHUB_TOKEN` | Automatic | Pre-configured by GitHub |
| `KUBECONFIG_DEV` | Dev cluster config | `cat ~/.kube/config \| base64` |
| `KUBECONFIG_STAGING` | Staging cluster config | Same as above |
| `KUBECONFIG_PROD` | Production cluster config | Same as above |

#### Optional Secrets

| Secret Name | Purpose | Required For |
|------------|---------|--------------|
| `FOSSA_API_KEY` | License scanning | security.yml |
| `CONAN_TOKEN` | C++ dependency auth | build-test.yml |
| `SLACK_WEBHOOK` | Deployment notifications | deploy.yml |

### Step 3: Configure Environments

1. Go to **Settings** → **Environments**
2. Create three environments:
   - `dev` (no protection rules)
   - `staging` (add reviewers)
   - `production` (add reviewers + deployment branch = main only)

#### Staging Configuration
- **Required reviewers**: 1
- **Wait timer**: 0 minutes
- **Deployment branches**: main, develop

#### Production Configuration
- **Required reviewers**: 2
- **Wait timer**: 10 minutes (optional cooldown)
- **Deployment branches**: main only

### Step 4: Test the Pipeline

#### 4.1 Test Build & Test Workflow

```bash
# Make a small change
echo "# Test" >> README.md

# Commit and push
git add README.md
git commit -m "test: Trigger CI/CD pipeline"
git push origin main

# Watch the workflow
# Go to: https://github.com/YOUR_USERNAME/code_architect/actions
```

#### 4.2 Test Manual Deployment

1. Go to **Actions** → **Deploy to Kubernetes**
2. Click **Run workflow**
3. Select:
   - Environment: `dev`
   - Services: `widget-core`
   - Version: `latest`
4. Click **Run workflow**
5. Monitor progress

### Step 5: Verify Deployment

```bash
# Check deployment status
kubectl get pods -n dev

# Check service logs
kubectl logs -n dev -l app=widget-core --tail=50

# Verify running
kubectl get svc -n dev
```

## Usage

### Building and Testing

**Automatic**: Every push to main/develop/feature branches triggers build & test.

**Manual**:
```bash
# Go to Actions → Build and Test → Run workflow
```

### Deploying Services

#### Deploy to Dev (Automatic)
```bash
# Simply push to main
git push origin main
```

#### Deploy to Staging (Manual)
1. Go to **Actions** → **Deploy to Kubernetes**
2. Click **Run workflow**
3. Select:
   - Environment: `staging`
   - Services: `widget-core,orb-core,xrc-service` (or `all`)
   - Version: `v1.0.0` (or commit SHA)
4. Click **Run workflow**
5. Approve deployment (requires reviewer approval)

#### Deploy to Production (Manual with Canary)
1. **Same as staging**, but select `prod`
2. **Two reviewers must approve**
3. Pipeline performs canary deployment:
   - 10% traffic for 10 minutes
   - Monitor error rates
   - Gradually increase to 50%, then 100%
   - Automatic rollback if error rate > threshold

### Rolling Back

```bash
# Via GitHub Actions
1. Go to Actions → Deploy to Kubernetes
2. Run workflow with:
   - Environment: prod
   - Services: widget-core
   - Version: rollback

# Via kubectl (manual)
kubectl rollout undo deployment/widget-core -n prod
kubectl rollout status deployment/widget-core -n prod
```

### Viewing Results

#### Build Results
- **Actions tab**: See workflow runs
- **Artifacts**: Download test reports, coverage
- **PR Comments**: Automated summary on PRs

#### Security Results
- **Security tab**: View all security findings
- **SARIF uploads**: Integrated with GitHub Code Scanning
- **PR Comments**: Security summary on PRs

#### Deployment Status
- **Environments tab**: See deployment history
- **Actions tab**: Deployment logs
- **kubectl**: Live cluster status

## Workflow Customization

### Modify Build Steps

Edit `.github/workflows/build-test.yml`:

```yaml
# Example: Add new service to build matrix
strategy:
  matrix:
    service: [widget-core, xrc-service, new-service]
```

### Modify Security Scans

Edit `.github/workflows/security.yml`:

```yaml
# Example: Add custom security check
- name: Custom Security Check
  run: |
    ./scripts/my-security-scan.sh
```

### Modify Deployment Strategy

Edit `.github/workflows/deploy.yml`:

```yaml
# Example: Change canary traffic split
kubectl patch deployment $service -n prod -p '{"spec":{"replicas":2}}'  # 20%
```

## Troubleshooting

### Build Failures

**Problem**: C++ service won't compile

```bash
# Check workflow logs for detailed error
# Fix locally first:
cd services/widget-core
mkdir build && cd build
cmake .. && make

# Then push fix
```

**Problem**: Docker image build fails

```bash
# Test locally:
cd services/widget-core
docker build -f Dockerfile.minimal -t widget-core:test .

# Check logs:
docker build --progress=plain -f Dockerfile.minimal .
```

### Security Scan Failures

**Problem**: High/critical vulnerabilities found

1. Check **Security** tab for details
2. Update dependencies:
   ```bash
   # For C++ (Conan)
   conan install . --update

   # For Node.js
   npm audit fix
   ```
3. Commit fixes and re-run

**Problem**: False positive secrets detected

```bash
# Add to .gitleaks.toml allowlist:
[allowlist]
paths = [
  '''path/to/test/file\.txt''',
]
```

### Deployment Failures

**Problem**: Deployment stuck in "Pending"

```bash
# Check pod status
kubectl describe pod -n dev <pod-name>

# Common causes:
# - Image pull errors (check registry auth)
# - Resource limits (increase requests/limits)
# - Node capacity (scale cluster)
```

**Problem**: Service not responding

```bash
# Check logs
kubectl logs -n dev -l app=widget-core --tail=100

# Check events
kubectl get events -n dev --sort-by='.lastTimestamp'

# Port-forward to test
kubectl port-forward -n dev svc/widget-core 50051:50051
```

### Permission Errors

**Problem**: GHCR push fails

1. Check **Settings** → **Actions** → **Workflow permissions**
2. Enable **Read and write permissions**
3. Re-run workflow

**Problem**: Kubernetes deployment fails

1. Check `KUBECONFIG_*` secret is base64 encoded
2. Verify cluster connectivity
3. Check service account permissions

## Monitoring & Observability

### GitHub Actions Metrics

- **Success Rate**: Track in Actions tab
- **Build Time**: View in workflow logs
- **Queue Time**: Check runner availability

### Deployment Metrics

```bash
# Pod health
kubectl get pods -n dev -w

# Resource usage
kubectl top pods -n dev
kubectl top nodes

# Service metrics
kubectl port-forward -n dev svc/widget-core 50051:50051
# Access metrics endpoint
```

### Security Metrics

- **Vulnerability Count**: Security tab
- **SBOM Coverage**: Check artifacts
- **Secret Scan Results**: workflow logs

## Best Practices

### For Developers

1. ✅ **Always create feature branches**
   ```bash
   git checkout -b feature/my-feature
   ```

2. ✅ **Run tests locally before pushing**
   ```bash
   cd test_stabilize && npm test
   ```

3. ✅ **Keep commits small and focused**
   ```bash
   git commit -m "fix: Correct widget rendering bug"
   ```

4. ✅ **Review security scan results**
   - Check PR comments
   - Fix issues before merging

5. ✅ **Tag releases**
   ```bash
   git tag -a v1.0.0 -m "Release v1.0.0"
   git push origin v1.0.0
   ```

### For Operations

1. ✅ **Monitor production deployments closely**
2. ✅ **Use canary deployments for risky changes**
3. ✅ **Keep rollback plan ready**
4. ✅ **Test in staging before production**
5. ✅ **Document deployment procedures**

### For Security

1. ✅ **Review all HIGH/CRITICAL findings**
2. ✅ **Keep dependencies updated**
3. ✅ **Rotate secrets regularly**
4. ✅ **Monitor security tab daily**
5. ✅ **Conduct periodic security audits**

## Metrics & KPIs

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Build Time | <5 min | TBD | ⏳ |
| Test Coverage | >80% | 95.45% | ✅ |
| Security Issues (HIGH) | 0 | TBD | ⏳ |
| Deployment Frequency | 5×/week | TBD | ⏳ |
| MTTR (Mean Time to Repair) | <1 hour | TBD | ⏳ |
| Change Failure Rate | <15% | TBD | ⏳ |

## Next Steps

### Immediate (Week 1)
- [ ] Push code to GitHub
- [ ] Configure secrets
- [ ] Run first build
- [ ] Deploy to dev environment

### Short-term (Week 2-4)
- [ ] Configure staging environment
- [ ] Set up monitoring (Prometheus/Grafana)
- [ ] Add integration tests
- [ ] Performance benchmarking

### Long-term (Month 2-3)
- [ ] Production deployment
- [ ] Implement GitOps (ArgoCD/Flux)
- [ ] Add chaos engineering tests
- [ ] Advanced observability (tracing, APM)

## Support & Resources

- **GitHub Actions Docs**: https://docs.github.com/en/actions
- **Kubernetes Docs**: https://kubernetes.io/docs/
- **Security Best Practices**: https://github.com/ossf/scorecard
- **Team Contact**: @CodeArchitect, @DevOps_Engineer

## Version History

- **v1.0.0** (2025-11-06): Initial CI/CD pipeline configuration
  - Build & test workflow
  - Security scanning workflow
  - Deployment workflow (dev/staging/prod)
  - Comprehensive documentation

---

**Status**: ✅ Ready to use - Push to GitHub and start deploying!
