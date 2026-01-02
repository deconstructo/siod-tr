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
- Actually use advanced programming concepts from "back then" (like concurrency in Lisp)
- Reclaim missed opportunities from the 1990s
- Make things work the way they *should* have


## What's Been Done
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
	- quarternions _useful_, octonions _not useful, but fun_
	- symbolic maths using symengine

## Building

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt install libgd-dev libsqlite3-dev p

# Build
make linux

# Run tests
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-gd.scm
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-sqlite3.scm
LD_LIBRARY_PATH=. ./siod -v01,-m2 tests/test-sql-utilities.scm
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
