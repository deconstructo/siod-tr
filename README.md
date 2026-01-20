# SIOD-TR: SIOD - The Reawakening

**Modernized SIOD for mathematical mayhem and computational chaos.**

## What is this?

This is SIOD 3.6.2 by George Carrette, dragged kicking and screaming into 2025.

Built from the original source at: https://people.delphiforums.com/gjc//siod.html

## Why?

Let's be honest: This is about bringing old tech back to life so you can do _bad_ things to it.

The goal is a proper mathematical computing environment where you can:
- Explore baroque number systems (ℂ → ℍ → 𝕆)
- Run mathematical simulations without low-level nonsense
- Symbolic mathematics
- Actually use advanced programming concepts from "back then" (like concurrency in Lisp)
- Reclaim missed opportunities from the 1990s
- Make things work the way they *should* have


## What's Been Done
** Octonion support**:
	- phase 1 done. octonions are now integrated as first class citizens
      into the siod core.
	- octonions do not participate in the polymorphic maths functions
      as they are not associative under multiplication 
	  and division is _strange_
	- requires the standalone LibOct library: (https://github.com/deconstructo/LibOctonion)
	- as i get my head around the octonions better, LibOctonion and siod-tr
	  support will be extended

**Quaternion support**:
	- quaternions are now integrated as first class citizens into
	  the siod core.
	- graceful handling of expressions that include floats, complex 
	  numbers and quaternions
	- requires CQRlib (https://sourceforge.net/projects/cqrlib/)

**Symengine support**:
	- added initial support for Symengine 
	- presently only works with floats, but this will be extended
	- requires symengine and libgmp to build

**Cleanup of legacy cruft**:
	- removed support for ancient databases
	- cleanup of legacy build targets in makefile

**Sqlite3 support**:
	- requires sqlite3 to be installed
	- replaces the legacy database types

**Readline support**:
	- added GNU readline support for interactive sessions
	- requires readline to be installed
	- This _really_ makes development so much more pleasant!

**JSON support**:
	- added support for reading/writing and interacting with data
	  in JSON format
	- built on cJSON

**GD Uplift**:
	- a minimal uplift of libGD support from v1.x to v2.3.3+
	- It's not a huge priority for me, but it has been tested and
          demonstrated to work

**Raylib support**:
	- Initial support for Raylib
	- check out solar-system.scm in the examples dir

**Complex maths**:
	- the siod core has been extended to support complex maths
	- requires C99 standard maths library
	- the standard maths functions are polymorphic, ie.
	  (+ 1 (make-rectanguar 3 4)) works happily.
	- TODO: I'd love more natural representations of complex numbers
	  i.e. (define a 12.45+4i) returning 12.45+4i	

**PlPlot support**:
	- Now we can do plots to files! 
	- requires plplot to be installed

## What's Next

	- updating of all of the documentation for the sysem
	- replacing the siod.html with a markdown version

## Building


git submodule update --init --recursive


```bash
# Install dependencies (Ubuntu/Debian)
sudo apt-get install -y \
    build-essential \
    libtool-bin \
    automake \
    pkg-config \
    libgd-dev \
    libsqlite3-dev \
    libcjson-dev \
    libreadline-dev \
    libgmp-dev \
    cmake \
    libplplot-dev \
    git

# Install dependencies (Fedora/RHEL)
sudo dnf install -y \
    @development-tools \
    libtool \
    automake \
    pkgconfig \
    gd-devel \
    sqlite-devel \
    cjson-devel \
    readline-devel \
    gmp-devel \
    cmake \
    plplot-devel \
    git



# Build submodules first
cd CQRlib && make all && cd ..
cd LibOctonion && make && cd ..
cd symengine && mkdir -p build && cd build && cmake .. && make && cd ../..

# Build raylib as shared library (IMPORTANT: use BUILD_SHARED_LIBS=ON)
cd raylib && mkdir -p build && cd build && \
  cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=OFF .. && \
  make && \
  ln -sf libraylib.so.*.*.* libraylib.so && \
  cd ../..

# Build SIOD
make linux

# Set up library path (required for running)
export LD_LIBRARY_PATH=.:CQRlib/lib/.libs:LibOctonion:symengine/build/symengine:raylib/build/raylib:$LD_LIBRARY_PATH

# Run the interpreter
./siod

# Or run with raylib support
./siod-raylib examples/raylib-examples.scm

# Run tests
./siod -v01,-m2 tests/test-gd.scm
./siod -v01,-m2 tests/test-sqlite3.scm
./siod -v01,-m2 tests/test-sql-utilities.scm
```

## Documentation

See `docs/` directory

Utility libraries:
- `gd-utilities.scm` - High-level graphics helpers
- `sql_sqlite3-utilities.scm` - High-level database helpers

## Project Structure

```
siod-tr/
├── attic/              # Old README and Makefile have been moved here 
├── docs/               # Module documentation
├── examples/           # Example scheme code for the various extensions
├── tests/        	# Test suites
├── *-utilities.scm     # High-level helper libraries
└── Makefile            # Build system (updated)
```

## Philosophy

> "The long dark night of the soul" with SNOBOL4 taught us that macro-based architectures translated from assembly create insurmountable barriers to mathematical extensions. 
>
> So we're doing it properly this time.

This project is about:
- High-level concepts over low-level details
- Mathematical exploration over implementation drudgery
- Making things work the way they should
- Playful chaos over rigid structure
- Reclaiming lost computational opportunities

## Original SIOD

SIOD (Scheme In One Defun/Day) was created by George Carrette in the 1990s as a small, embeddable Scheme interpreter. The original is a marvel of compact design and remains a testament to elegant minimalism in language implementation.

We're just... giving it new life with a bit of chaos goblin energy.

## License

GPL-3.0 - respects George Carrette's original permissive license while ensuring future modifications remain open.

Original SIOD by George Carrette: https://people.delphiforums.com/gjc//siod.html

## Contributing

Got ideas for Scáthifying this further? Pull requests welcome.

Especially interested in:
- RayLib bindings
- Quaternion/Octonion implementations
- Mathematical simulation examples
- Making it weirder
- Improving documentation
- cleaning up the build directory

## Credits

- **George Carrette**: Original SIOD author
- **Scáth**: Chaos goblin modernization, mathematical computing focus
- **DeeDeeCee**: SQLite3 bindings (2025-12-23)

---

*"Enjoy the pointlessness!" - but make it mathematical*
