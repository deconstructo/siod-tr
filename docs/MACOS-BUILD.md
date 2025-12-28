# Building SIOD-TR on macOS

## Will It Work?

**YES!** ✅ The pthreads code uses standard POSIX APIs that macOS fully supports.

The only differences are in the build system:
- Dynamic libraries use `.dylib` instead of `.so`
- Linker flags are different
- Library paths may differ

## Prerequisites

Install dependencies via Homebrew:

```bash
# Install Homebrew if you don't have it
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install required libraries
brew install libgd sqlite3 pkg-config
```

## Building

```bash
# Clone or navigate to siod-tr directory
cd siod-tr

# Clean any previous build
make clean

# Build for macOS
make darwin
# or
make macos
```

## Potential Issues & Solutions

### Issue 1: pkg-config can't find libgd

If you see errors about gdlib:

```bash
# Find where Homebrew installed libgd
brew --prefix libgd

# Add to PKG_CONFIG_PATH
export PKG_CONFIG_PATH="$(brew --prefix libgd)/lib/pkgconfig:$PKG_CONFIG_PATH"
```

Or edit the Makefile and add explicit paths:

```makefile
gd.o: gd.c
	$(CC) $(CFLAGS) -I$(shell brew --prefix libgd)/include -c gd.c

gd.$(SO): gd.o libsiod.$(SO)
	$(LD) -o gd.$(SO) $(LD_LIB_FLAGS) gd.o libsiod.$(SO) \
	      -L$(shell brew --prefix libgd)/lib -lgd $(LD_LIB_LIBS)
```

### Issue 2: Can't find libsiod.dylib when running

macOS is stricter about library paths. Solutions:

**Option 1: Set DYLD_LIBRARY_PATH**
```bash
export DYLD_LIBRARY_PATH=.:$DYLD_LIBRARY_PATH
./siod -v01,-m2 test-pthreads.scm
```

**Option 2: Use install_name_tool** (permanent fix)
```bash
# After building, fix the library paths
install_name_tool -change libsiod.dylib @executable_path/libsiod.dylib ./siod
```

**Option 3: Install to system location**
```bash
sudo make install
# Then run without DYLD_LIBRARY_PATH
./siod -v01,-m2 test-pthreads.scm
```

### Issue 3: "dyld: Library not loaded" errors

This means the dynamic linker can't find libraries. Check paths:

```bash
# See what libraries siod expects
otool -L ./siod

# See what pthreads.dylib expects  
otool -L ./pthreads.dylib
```

Fix with install_name_tool or set DYLD_LIBRARY_PATH.

## Running Tests

Once built:

```bash
# Set library path
export DYLD_LIBRARY_PATH=.

# Run tests
./siod -v01,-m2 test-pthreads.scm
./siod -v01,-m2 test-gd.scm
./siod -v01,-m2 test-sqlite3.scm
```

## macOS-Specific Notes

### Security (Catalina+)

macOS may block unsigned binaries. If you see "cannot be opened because the developer cannot be verified":

```bash
# Remove quarantine attribute
xattr -d com.apple.quarantine ./siod
xattr -d com.apple.quarantine ./*.dylib
```

### Apple Silicon (M1/M2/M3)

The code should work on ARM64 (Apple Silicon) Macs:

```bash
# Check your architecture
uname -m
# x86_64 = Intel
# arm64 = Apple Silicon

# Build works the same way
make darwin
```

If you get architecture mismatches:

```bash
# Force ARM64 build
make darwin CC="clang -arch arm64" LD="clang -arch arm64"

# Or force x86_64 (Rosetta)
make darwin CC="clang -arch x86_64" LD="clang -arch x86_64"
```

### Homebrew Paths

Homebrew installs to different locations:

- **Intel Macs**: `/usr/local`
- **Apple Silicon**: `/opt/homebrew`

The build should auto-detect via `pkg-config`, but if not:

```bash
# Check Homebrew prefix
brew --prefix

# Add to paths
export PKG_CONFIG_PATH="$(brew --prefix)/lib/pkgconfig:$PKG_CONFIG_PATH"
export CPATH="$(brew --prefix)/include:$CPATH"
export LIBRARY_PATH="$(brew --prefix)/lib:$LIBRARY_PATH"
```

## What Definitely Works on macOS

✅ **pthreads module** - POSIX threads are native to macOS  
✅ **SQLite3** - Part of macOS system libraries  
✅ **All Scheme code** - Platform-independent  

Should work (with Homebrew):  
✅ **libgd graphics** - Via `brew install libgd`  

## Quick Start (macOS)

```bash
# One-time setup
brew install libgd sqlite3 pkg-config

# Build
cd siod-tr
make clean
make darwin

# Test
export DYLD_LIBRARY_PATH=.
./siod -v01,-m2 test-pthreads.scm
```

## Differences from Linux

| Feature | Linux | macOS |
|---------|-------|-------|
| Shared library ext | `.so` | `.dylib` |
| Linker flag | `-shared` | `-dynamiclib` |
| Runtime lib path | `LD_LIBRARY_PATH` | `DYLD_LIBRARY_PATH` |
| Dynamic linker | `-ldl` | (built-in) |
| Crypt library | `-lcrypt` | (not needed) |
| pthreads | ✅ | ✅ Same API! |

## Success Indicators

You'll know it worked when:

```bash
$ ./siod
SIOD: Scheme In One Defun/Day, Version 3.6.2 (SIOD-TR)
>

$ ./siod -v01,-m2 test-pthreads.scm
=== Pthreads Synchronization Test ===
[TEST 1] Mutex operations
  Created mutex: #<MUTEX 0x...>
...
=== All synchronization tests passed! ===
```

## Getting Help

If you encounter issues:

1. Check Homebrew installations: `brew list`
2. Verify pkg-config: `pkg-config --libs gdlib`
3. Check library paths: `otool -L ./siod`
4. Set environment: `export DYLD_LIBRARY_PATH=.`

The pthreads code is 100% portable - any issues will be build-system related and solvable!

---

**Bottom line**: The code will **absolutely work** on macOS. You just need to:
1. `brew install libgd sqlite3`
2. `make darwin`  
3. `export DYLD_LIBRARY_PATH=.`
4. Run tests!

🍎 Ready to Scáthify on macOS! 🔥
