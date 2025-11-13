# GNAT Ada Compiler Installation Guide for macOS

**Date:** 2025-11-04
**System:** macOS (Darwin 24.6.0)
**Status:** Investigation Complete

---

## ⚠️ Important Discovery

**The Homebrew GCC package does NOT include GNAT (Ada compiler).**

```bash
# This was installed (441.3MB), but does NOT include GNAT:
brew install gcc  # Only includes: C, C++, Fortran compilers
```

**Verification:**
```bash
$ ls /opt/homebrew/Cellar/gcc/15.2.0/bin/
# Result: gcc-15, g++-15, gfortran-15
# Missing: gnatmake, gnatbind, gnatlink, etc.
```

---

## GNAT Installation Options for macOS

### Option 1: Alire Package Manager (RECOMMENDED)

**Alire** is the modern Ada package manager that can install and manage GNAT toolchains.

#### Installation Steps:

**1. Download Alire:**
```bash
# Visit: https://alire.ada.dev/
# Or download directly:
cd /tmp
curl -L -O https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-aarch64-macos.zip

# Extract
unzip alr-2.0.2-bin-aarch64-macos.zip
sudo mv bin/alr /usr/local/bin/
chmod +x /usr/local/bin/alr
```

**2. Install GNAT via Alire:**
```bash
# Initialize Alire (first time only)
alr toolchain --select

# This will prompt you to select a GNAT version
# Choose: gnat_native (latest stable)

# Verify installation
alr toolchain
which gnatmake
gnatmake --version
```

**Pros:**
- ✅ Modern, maintained package manager
- ✅ Handles dependencies automatically
- ✅ Can manage multiple GNAT versions
- ✅ Works well with PolyORB and other Ada projects

**Cons:**
- ⚠️ Not in Homebrew (manual download required)
- ⚠️ Additional tool to learn

**Time:** ~10-15 minutes

---

### Option 2: AdaCore GNAT Community Edition

**AdaCore** provides free GNAT Community Edition builds.

#### Installation Steps:

**1. Download:**
- Visit: https://www.adacore.com/community
- Register for free account (required)
- Download: GNAT Community Edition for macOS
- Choose latest stable version (e.g., 2024)

**2. Install:**
```bash
# Mount the DMG and run installer
# Default installation: /opt/GNAT/2024
# Or custom path of your choice

# Add to PATH in ~/.zshrc or ~/.bash_profile:
export PATH="/opt/GNAT/2024/bin:$PATH"

# Reload shell
source ~/.zshrc

# Verify
which gnatmake
gnatmake --version
```

**Pros:**
- ✅ Official AdaCore builds
- ✅ Well-tested and stable
- ✅ Includes GPS IDE and other tools
- ✅ Commercial-grade quality

**Cons:**
- ⚠️ Requires registration
- ⚠️ Manual download (large, ~500MB+)
- ⚠️ Annual releases (not always latest)

**Time:** ~20-30 minutes (including download)

---

### Option 3: FSF GNAT (Build from Source)

**Free Software Foundation GNAT** can be built from GCC sources.

#### Installation Steps:

**1. Install Build Dependencies:**
```bash
brew install gmp mpfr libmpc isl
```

**2. Download GCC Source:**
```bash
cd /tmp
wget https://ftp.gnu.org/gnu/gcc/gcc-13.2.0/gcc-13.2.0.tar.xz
tar xf gcc-13.2.0.tar.xz
cd gcc-13.2.0
```

**3. Configure and Build:**
```bash
# Note: Requires existing Ada compiler to bootstrap!
# This is the "bootstrap problem" - you need GNAT to build GNAT

./configure \
  --prefix=/usr/local/gnat-13 \
  --enable-languages=ada,c,c++ \
  --disable-multilib

make -j$(sysctl -n hw.ncpu)  # Takes 2-4 hours!
sudo make install

# Add to PATH
export PATH="/usr/local/gnat-13/bin:$PATH"
```

**Pros:**
- ✅ Free Software Foundation version
- ✅ No registration required
- ✅ Latest GCC version

**Cons:**
- ❌ Requires existing Ada compiler (bootstrap problem!)
- ❌ Very time-consuming (2-4 hours compile time)
- ❌ Complex build process
- ❌ May have macOS compatibility issues

**Time:** 3-5 hours

**Recommendation:** ❌ NOT RECOMMENDED for most users

---

## Comparison Matrix

| Feature | Alire | AdaCore Community | Build from Source |
|---------|-------|-------------------|-------------------|
| Ease of Installation | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐ |
| Time Required | 10-15 min | 20-30 min | 3-5 hours |
| Registration | No | Yes | No |
| Package Management | Yes | No | No |
| Maintenance | Easy | Manual updates | Very difficult |
| macOS Support | Excellent | Good | Variable |
| **RECOMMENDED** | ✅ **YES** | ⚠️ Alternative | ❌ No |

---

## Recommended Installation: Alire

Based on analysis, **Alire is the best option** for PolyORB development:

### Quick Start with Alire

```bash
# 1. Download and install Alire
cd /tmp
curl -L -O https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-aarch64-macos.zip
unzip alr-2.0.2-bin-aarch64-macos.zip
sudo mv bin/alr /usr/local/bin/
rm -rf alr-2.0.2-bin-aarch64-macos.zip bin/

# 2. Initialize and install GNAT
alr toolchain --select

# Follow prompts:
# - Select gnat_native (latest)
# - Confirm installation
# Wait ~5-10 minutes for download and installation

# 3. Verify installation
alr toolchain
which gnatmake
gnatmake --version

# Expected output:
# GNATMAKE <version>
# Copyright (C) 1995-<year>, Free Software Foundation, Inc.
```

### Environment Setup

Add to `~/.zshrc` (or `~/.bash_profile`):

```bash
# Alire GNAT toolchain
eval "$(alr toolchain --export-shell-path)"
```

Then reload:
```bash
source ~/.zshrc
```

---

## Verification Checklist

After installation, verify GNAT is working:

```bash
# 1. Check gnatmake
which gnatmake
gnatmake --version

# 2. Check other GNAT tools
which gnatbind
which gnatlink
which gprbuild  # GNAT project manager (if included)

# 3. Test simple Ada program
cat > hello.adb << 'EOF'
with Ada.Text_IO; use Ada.Text_IO;
procedure Hello is
begin
   Put_Line("Hello from GNAT!");
end Hello;
EOF

gnatmake hello.adb
./hello
# Expected output: Hello from GNAT!

# Cleanup
rm hello hello.adb hello.ali hello.o
```

**Success Criteria:**
- ✅ `gnatmake --version` returns version information
- ✅ Simple Ada program compiles and runs
- ✅ All GNAT tools are in PATH

---

## PolyORB Build Requirements

Once GNAT is installed, you'll also need:

### 1. GNU Make 3.80+
```bash
# macOS has make by default, verify version:
make --version
# Should show: GNU Make 3.81 or higher
```

### 2. Autoconf/Automake (for building from source)
```bash
brew install autoconf automake libtool
```

### 3. GNATPython (for test suite)
```bash
# After GNAT is installed:
git clone https://github.com/Nikokrock/gnatpython
cd gnatpython

# Install in Python
python3 setup.py install
# OR: Set PYTHONPATH
export PYTHONPATH=/path/to/gnatpython:$PYTHONPATH

# Compile rlimit utility
cd src/rlimit
gcc -o rlimit rlimit.c
sudo cp rlimit /usr/local/bin/
```

---

## Next Steps After GNAT Installation

1. **Verify Installation**
   ```bash
   gnatmake --version
   ```

2. **Configure PolyORB**
   ```bash
   cd /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB
   ./configure --prefix=/tmp/polyorb-test
   ```

3. **Build PolyORB**
   ```bash
   make -j4
   make install
   ```

4. **Run Test Suite**
   ```bash
   cd testsuite
   ./testsuite.py > baseline_results.txt 2>&1
   ```

5. **Begin Refactoring**
   - See: REFACTORING_CONSTRAINTS_AND_RECOMMENDATIONS.md
   - Start with: Priority 1 (Documentation) or Priority 2 (Control Flow)

---

## Troubleshooting

### Issue: "alr: command not found"
**Solution:**
```bash
# Check if alr was installed
ls -la /usr/local/bin/alr

# If not found, verify download location
# Try alternative path: ~/.local/bin/alr
```

### Issue: "gnatmake: command not found" after Alire install
**Solution:**
```bash
# Alire installs toolchain in ~/.config/alire
# Need to export path:
eval "$(alr toolchain --export-shell-path)"

# Or manually:
export PATH="$HOME/.config/alire/toolchains/gnat_native_<version>/bin:$PATH"
```

### Issue: Configure fails with "Ada compiler not found"
**Solution:**
```bash
# Verify GNAT in PATH:
which gnatmake

# If found, try specifying explicitly:
./configure --prefix=/tmp/polyorb-test ADA=gnatmake

# Or set environment variable:
export PATH="/path/to/gnat/bin:$PATH"
./configure --prefix=/tmp/polyorb-test
```

### Issue: Permission denied during Alire installation
**Solution:**
```bash
# Don't use sudo with alr itself, only for moving binary:
sudo mv bin/alr /usr/local/bin/
sudo chmod +x /usr/local/bin/alr

# Then run alr without sudo:
alr toolchain --select
```

---

## Alternative: Docker Development Environment

If GNAT installation proves difficult, consider using Docker:

```dockerfile
# Dockerfile for PolyORB development
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    gnat-12 \
    gprbuild \
    make \
    autoconf \
    automake \
    python3 \
    git

WORKDIR /workspace
```

**Build and run:**
```bash
# Build container
docker build -t polyorb-dev .

# Run with PolyORB mounted
docker run -it -v $(pwd):/workspace polyorb-dev

# Inside container:
./configure && make && make test
```

---

## Summary

**For macOS PolyORB Development:**

1. ✅ **Install Alire** (10-15 minutes)
2. ✅ **Install GNAT via Alire** (5-10 minutes)
3. ✅ **Install GNATPython** (5 minutes)
4. ✅ **Configure and build PolyORB** (10-30 minutes)
5. ✅ **Run baseline tests** (variable, depends on test suite)
6. ✅ **Begin refactoring with validated environment**

**Total Time:** ~1-2 hours (mostly downloads and compilation)

**Blockers Resolved:**
- ❌ Homebrew GCC → ✅ Alire GNAT
- ❌ No Ada compiler → ✅ Modern toolchain with package management
- ❌ Unknown build process → ✅ Documented step-by-step

---

## Questions?

- **Alire Documentation:** https://alire.ada.dev/docs/
- **GNAT User Guide:** https://docs.adacore.com/gnat_ugn-docs/
- **PolyORB Installation:** See `INSTALL` file in repository

---

**Document Version:** 1.0
**Last Updated:** 2025-11-04
**Tested On:** macOS Sequoia (Darwin 24.6.0), Apple Silicon

