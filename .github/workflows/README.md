# GitHub Actions Workflows

This directory contains all CI/CD workflows for the RefactorTeam project.

## Workflows

### 🔨 build-test.yml
**Purpose**: Build, test, and create Docker images

**Triggers**:
- Push to main/develop/feature branches
- Pull requests
- Manual dispatch

**What it does**:
- Compiles C++ services (widget-core, xrc-service)
- Compiles Ada services (orb-core)
- Runs Jest unit tests + mutation tests
- Builds Docker images
- Runs security scans with Trivy
- Deploys to test cluster for integration tests
- Generates coverage and test reports

**Runtime**: ~15-20 minutes

### 🔒 security.yml
**Purpose**: Comprehensive security scanning

**Triggers**:
- Push to main/develop/feature branches
- Pull requests
- Daily at 2 AM UTC

**What it does**:
- Secret scanning (TruffleHog, GitLeaks)
- SAST (cppcheck, clang-tidy, Semgrep)
- Container vulnerability scanning (Trivy, Grype)
- SBOM generation
- Dependency scanning
- License compliance
- Security policy validation
- OpenSSF Scorecard

**Runtime**: ~25-30 minutes

### 🚀 deploy.yml
**Purpose**: Deploy services to Kubernetes

**Triggers**:
- Manual dispatch (any environment)
- Automatic on push to main (dev only)

**What it does**:
- Validates K8s manifests
- Deploys to dev/staging/prod
- Blue-green deployment (staging)
- Canary deployment (production)
- Rollback capability
- Post-deployment verification

**Runtime**:
- Dev: ~5 minutes
- Staging: ~10 minutes
- Prod: ~20 minutes (with canary)

## Quick Commands

### Trigger Build Manually
```bash
gh workflow run build-test.yml
```

### Deploy to Development
```bash
gh workflow run deploy.yml -f environment=dev -f services=all
```

### Deploy to Staging
```bash
gh workflow run deploy.yml -f environment=staging -f services=widget-core -f version=v1.0.0
```

### Deploy to Production
```bash
gh workflow run deploy.yml -f environment=prod -f services=all -f version=v1.0.0
```

### Rollback Production
```bash
gh workflow run deploy.yml -f environment=prod -f services=widget-core -f version=rollback
```

## Workflow Status Badges

Add to your README.md:

```markdown
![Build and Test](https://github.com/YOUR_USERNAME/code_architect/actions/workflows/build-test.yml/badge.svg)
![Security Scan](https://github.com/YOUR_USERNAME/code_architect/actions/workflows/security.yml/badge.svg)
```

## Secrets Required

| Secret | Used By | Description |
|--------|---------|-------------|
| `GITHUB_TOKEN` | All | Auto-provided by GitHub |
| `KUBECONFIG_DEV` | deploy.yml | Dev cluster access |
| `KUBECONFIG_STAGING` | deploy.yml | Staging cluster access |
| `KUBECONFIG_PROD` | deploy.yml | Prod cluster access |
| `FOSSA_API_KEY` | security.yml | License scanning |

## Troubleshooting

### Build fails on dependency installation
- Check service Dockerfiles
- Verify package versions
- Look for network issues in logs

### Security scan flags false positives
- Update `.gitleaks.toml` allowlist
- Add exceptions to security policies

### Deployment fails
- Verify kubeconfig secrets are base64 encoded
- Check cluster connectivity
- Review pod logs: `kubectl logs -n <namespace> <pod-name>`

## Monitoring

- **Workflow runs**: GitHub Actions tab
- **Security findings**: Security tab
- **Deployments**: Environments tab
- **Artifacts**: Available for 7-90 days

## Documentation

See [CI-CD-SETUP-GUIDE.md](../CI-CD-SETUP-GUIDE.md) for complete setup instructions.
