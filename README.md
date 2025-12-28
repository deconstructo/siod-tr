# SIOD-TR: SIOD - The Reawakening

**Modernized SIOD for mathematical mayhem and computational chaos.**

## What is this?

This is SIOD 3.6.2 by George Carrette, dragged kicking and screaming into 2025.

Built from the original source at: https://people.delphiforums.com/gjc//siod.html

## Why?

Let's be honest: This is about bringing old tech back to life so you can do _bad_ things to it.

You know, **Scáthify** it. 

The goal is a proper mathematical computing environment where you can:
- Explore baroque number systems (ℂ → ℍ → 𝕆)
- Run mathematical simulations without low-level nonsense
- Actually use advanced programming concepts from "back then" (like concurrency in Lisp)
- Reclaim missed opportunities from the 1990s
- Make things work the way they *should* have

## What's "Scáthify"?

Take perfectly good vintage software and:
1. Rip out the legacy cruft (bye MySQL, Sybase, Oracle, NDBM)
2. Modernize what's useful (hello libgd 2.3.3, SQLite3)
3. Fix security holes (mkstemp > mktemp)
4. Actually document things
5. Add the mathematical tools you actually need
6. Do it with a chaotic goblin energy

## What's Been Done

### Modernizations
- **GD Graphics**: Updated from libgd 1.2 (1996) to 2.3.3 (2025)
  - Modern font access via `gdFontGet*()` functions
  - Fixed security vulnerability (mktemp → mkstemp)
  - Comprehensive documentation and utilities
  - Test suite with sample outputs

- **SQLite3**: Full database support
  - Prepared statements with parameter binding
  - High-level utility library for common operations
  - Complete API documentation
  - Working demo and test suite

- **Removed Legacy Cruft**:
  - mSQL bindings (obsolete)
  - Sybase bindings (obsolete)
  - Oracle bindings (obsolete)
  - NDBM (replaced by SQLite3)

- **Build System**: Updated to use pkg-config for modern library detection

- **Documentation**: Created `docs/` with proper module references

- **Tests**: Add test suites for new and updated modules

### Security Fixes
- Replaced unsafe `mktemp()` with `mkstemp()` (eliminated race condition)
- All compiler warnings addressed

## What's Next

The roadmap to mathematical chaos:

1. **Interactive Graphics**: SDL2 or RayLib bindings for real-time visualization
2. **Baroque Number System**: Complex → Quaternion → Octonion progression
3. **Symbolic Algebra**: SymEngine integration
4. **Concurrency**: Proper threading/async (the Lisp way)
5. **Mathematical Simulations**: Gravity systems, attractors, chaos theory

## Building

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt install libgd-dev libsqlite3-dev

# Build
make linux

# Run tests
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-gd.scm
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-sqlite3.scm
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-sql-utilities.scm
```

## Documentation

See `docs/` directory:
- `gd.md` - GD graphics library reference
- `sql_sqlite3.md` - SQLite3 database operations

Utility libraries:
- `gd-utilities.scm` - High-level graphics helpers
- `sql_sqlite3-utilities.scm` - High-level database helpers

## Project Structure

```
siod-tr/
├── docs/               # Module documentation
├── gd.c                # Graphics bindings (modernized)
├── sql_sqlite3.c       # Database bindings
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
- SDL2/RayLib bindings
- Quaternion/Octonion implementations
- SymEngine integration
- Mathematical simulation examples
- Making it weirder

## Credits

- **George Carrette**: Original SIOD author
- **Scáth**: Chaos goblin modernization, mathematical computing focus
- **DeeDeeCee**: SQLite3 bindings (2025-12-23)

---

*"Enjoy the pointlessness!" - but make it mathematical*
