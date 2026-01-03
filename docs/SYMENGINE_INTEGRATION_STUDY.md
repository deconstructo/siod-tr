# SymEngine Integration for SIOD-TR
## Comprehensive Feasibility Study and Design Document

---

## Executive Summary

**Verdict: HIGHLY FEASIBLE** ✅

SymEngine integration with SIOD-TR is not only feasible but an excellent fit for the "baroque mathematics" vision. The library provides:
- **Fast C++ core** with clean C wrapper API
- **MIT License** (compatible with SIOD)
- **Active development** (used by SageMath, SymPy)
- **Perfect feature set** for mathematical exploration

**Timeline Estimate:** 2-3 weeks for full integration
**Complexity:** Similar to PLplot integration
**Value:** Transforms SIOD-TR into complete symbolic mathematics environment

---

## 1. What is SymEngine?

### Core Features

SymEngine is a standalone fast symbolic manipulation library written in C++:

- **Symbolic algebra**: Expand, simplify, factor expressions
- **Calculus**: Differentiation (YES), Series expansion (YES)
- **Integration**: LIMITED (basic cases only, not production-ready)
- **Equation solving**: Linear systems, polynomial equations
- **Matrices**: Symbolic matrix operations
- **Special functions**: Gamma, Beta, Bessel, etc.
- **Numerical evaluation**: Convert symbolic → numeric

### Performance

- **10-100x faster** than SymPy (Python)
- Designed for speed-critical applications
- Used in production scientific computing

### C API (`cwrapper.h`)

The C wrapper provides complete access to SymEngine functionality:
- All operations available through C functions
- Manual memory management via `basic` objects
- Reference counting handled by library
- Thread-safe option available

---

## 2. Integration Feasibility Analysis

### ✅ Strong Advantages

#### 1. **Clean C API**
```c
#include <symengine/cwrapper.h>

basic x, y, result;
basic_new_stack(x);
basic_new_stack(y);
basic_new_stack(result);

symbol_set(x, "x");
integer_set_si(y, 2);
basic_pow(result, x, y);  // x^2
```

The C API is well-documented and straightforward to wrap.

#### 2. **SIOD Integration Pattern**
Follows same pattern as PLplot:
```c
// siod_symengine.c
LISP siod_sym_diff(LISP expr, LISP var) {
    basic e = lisp2basic(expr);
    basic v = lisp2basic(var);
    basic result;
    basic_new_stack(result);
    basic_diff(result, e, v);
    return basic2lisp(result);
}
```

#### 3. **Natural S-Expression Mapping**
```scheme
; Scheme expression
(+ (* x x) (* 2 x) 1)

; Maps naturally to SymEngine tree
Add(Mul(x, x), Mul(2, x), 1)
```

#### 4. **No External Dependencies**
SymEngine is self-contained (includes Teuchos RCP internally).

#### 5. **Package Availability**
```bash
# Build from source (recommended for optimization)
git clone https://github.com/symengine/symengine
cd symengine
cmake . && make && sudo make install
```

### ⚠️ Limitations

#### 1. **Integration Not Production-Ready**
- Only basic integration patterns implemented
- For complex integrals, would need external tool (Maxima, SymPy)
- **Mitigation**: Focus on differentiation, series, simplification

#### 2. **Memory Management**
- Must manually manage `basic` objects
- Reference counting can leak if not careful
- **Mitigation**: Create RAII wrappers, thorough testing

#### 3. **Error Handling**
- C API returns error codes
- Must check every operation
- **Mitigation**: Wrapper functions with proper error handling

#### 4. **Build Complexity**
- C++ library (requires C++ compiler)
- Multiple optional dependencies (GMP, MPFR, FLINT)
- **Mitigation**: Minimal build, document clearly

---

## 3. Proposed Scheme API Design

### Philosophy: Natural and Lisp-like

The API should feel like native Scheme, not like calling into C.

### Core Design Patterns

#### Pattern 1: S-Expressions as Symbolic Expressions

```scheme
; Scheme list notation IS the symbolic expression
(define expr '(+ (* x x) (* 2 x) 1))

; Create symbolic expression
(sym expr)  ; => <symbolic: x^2 + 2*x + 1>
```

#### Pattern 2: Functional Operations

```scheme
; Differentiation
(sym-diff '(* x x) 'x)  ; => <symbolic: 2*x>

; Evaluation
(sym-eval '(+ x 1) '((x . 5)))  ; => 6

; Simplification
(sym-simplify '(+ (* x 0) (* x 1)))  ; => <symbolic: x>
```

#### Pattern 3: Integration with Numerical Computing

```scheme
; Create function from symbolic expression
(define f (sym->lambda '(* x x) '(x)))
(f 5)  ; => 25

; Plot symbolic function
(plot-function-symbolic '(sin x) 'x 0 6.28)
```

---

## 4. Complete API Specification

### 4.1 Core Expression Creation

```scheme
;;; (sym expr) - Create symbolic expression from S-expression
;;; Examples:
(sym 'x)                    ; => <symbolic: x>
(sym '(+ x 1))              ; => <symbolic: x + 1>
(sym '(sin (* 2 x)))        ; => <symbolic: sin(2*x)>
(sym 5)                     ; => <symbolic: 5>
(sym 3.14)                  ; => <symbolic: 3.14>

;;; (sym-symbol name) - Create symbolic variable
(sym-symbol "x")            ; => <symbolic: x>
(sym-symbol "theta")        ; => <symbolic: theta>

;;; (sym-number n) - Create symbolic number
(sym-number 42)             ; => <symbolic: 42>
(sym-number 1/3)            ; => <symbolic: 1/3>
```

### 4.2 Algebraic Operations

```scheme
;;; Standard arithmetic - works on symbolic expressions
(sym+ '(* x 2) '(* x 3))    ; => <symbolic: 5*x>
(sym- '(* x 5) '(* x 2))    ; => <symbolic: 3*x>
(sym* 'x 'y)                ; => <symbolic: x*y>
(sym/ 'x 'y)                ; => <symbolic: x/y>
(sym-pow 'x 2)              ; => <symbolic: x^2>

;;; Simplification
(sym-simplify '(+ (* x 0) (* x 1)))        ; => <symbolic: x>
(sym-expand '(* (+ x 1) (+ x 2)))          ; => <symbolic: x^2 + 3*x + 2>
(sym-factor '(+ (* x x) (* 2 x) 1))        ; => <symbolic: (x + 1)^2>
```

### 4.3 Calculus

```scheme
;;; Differentiation
(sym-diff '(* x x) 'x)                     ; => <symbolic: 2*x>
(sym-diff '(sin x) 'x)                     ; => <symbolic: cos(x)>
(sym-diff '(* (exp x) (sin x)) 'x)         ; => <symbolic: exp(x)*sin(x) + exp(x)*cos(x)>

;;; Higher-order derivatives
(sym-diff-n '(* x x x x) 'x 2)             ; => <symbolic: 12*x^2>

;;; Partial derivatives (for multi-variable)
(sym-diff '(+ (* x x) (* y y)) 'x)         ; => <symbolic: 2*x>

;;; Series expansion
(sym-series '(exp x) 'x 0 5)               ; => <symbolic: 1 + x + x^2/2 + x^3/6 + x^4/24 + O(x^5)>
(sym-series '(sin x) 'x 0 7)               ; => <symbolic: x - x^3/6 + x^5/120 + O(x^7)>
```

### 4.4 Evaluation and Substitution

```scheme
;;; Substitute values
(sym-subs '(+ (* x x) 1) '((x . 5)))       ; => <symbolic: 26>
(sym-subs '(* x y) '((x . 2) (y . 3)))     ; => <symbolic: 6>

;;; Numerical evaluation
(sym-evalf '(/ 22 7))                      ; => 3.142857...
(sym-evalf '(sin 1))                       ; => 0.8414709...

;;; Convert to lambda
(define f (sym->lambda '(* x x) '(x)))
(f 5)                                       ; => 25
(map f '(1 2 3 4 5))                       ; => (1 4 9 16 25)
```

### 4.5 Solving

```scheme
;;; Solve equation for variable
(sym-solve '(= (+ (* x x) (* -5 x) 6) 0) 'x)  ; => (2 3)

;;; Solve system of linear equations
(sym-solve-linear '((= (+ (* 2 x) y) 5)
                    (= (- x y) 1))
                  '(x y))                      ; => ((x . 2) (y . 1))
```

### 4.6 Special Functions

```scheme
;;; Trigonometric
(sym '(sin x))                             ; => <symbolic: sin(x)>
(sym '(cos x))                             ; => <symbolic: cos(x)>
(sym '(tan x))                             ; => <symbolic: tan(x)>

;;; Exponential/Logarithmic
(sym '(exp x))                             ; => <symbolic: exp(x)>
(sym '(log x))                             ; => <symbolic: log(x)>

;;; Constants
(sym 'pi)                                  ; => <symbolic: π>
(sym 'e)                                   ; => <symbolic: e>
```

### 4.7 Output and Display

```scheme
;;; String representation
(sym->string '(+ (* x x) 1))               ; => "x^2 + 1"

;;; Pretty printing
(sym-display '(/ (+ x 1) (- x 1)))         ; Prints: (x + 1)/(x - 1)

;;; LaTeX output (for papers!)
(sym->latex '(/ (+ x 1) (- x 1)))          ; => "\\frac{x + 1}{x - 1}"
```

---

## 5. PLplot Integration Examples

### Example 1: Plot Derivative

```scheme
(define (plot-with-derivative expr var x-min x-max)
  (let* ((f (sym->lambda expr (list var)))
         (df-expr (sym-diff expr var))
         (df (sym->lambda df-expr (list var)))
         (x-vals (range x-min x-max 0.1)))
    
    (plot-device "pdf")
    (plot-output "function-and-derivative.pdf")
    (plot-init)
    (plot-env x-min x-max -5.0 5.0)
    (plot-labels "x" "y" "Function and Derivative")
    
    ; Plot original function
    (plot-color 2)
    (plot-line x-vals (map f x-vals))
    
    ; Plot derivative
    (plot-color 4)
    (plot-line x-vals (map df x-vals))
    
    (plot-end)))

; Usage:
(plot-with-derivative '(* x x) 'x -3.0 3.0)
```

### Example 2: Taylor Series Visualization

```scheme
(define (plot-taylor-approximations expr var center orders)
  (plot-device "pdf")
  (plot-output "taylor-series.pdf")
  (plot-init)
  (plot-subplot 2 2)
  
  (let ((f (sym->lambda expr (list var)))
        (x-vals (range -3.0 3.0 0.05)))
    
    ; Plot original function
    (plot-env -3.0 3.0 -2.0 2.0)
    (plot-labels "x" "f(x)" "Original")
    (plot-color 1)
    (plot-line x-vals (map f x-vals))
    
    ; Plot each Taylor approximation
    (map (lambda (order)
           (plot-advance)
           (let* ((taylor (sym-series expr var center order))
                  (taylor-func (sym->lambda taylor (list var))))
             (plot-env -3.0 3.0 -2.0 2.0)
             (plot-labels "x" "T(x)" 
                         (string-append "Order " (number->string order)))
             (plot-color 1)
             (plot-line x-vals (map f x-vals))
             (plot-color 2)
             (plot-line x-vals (map taylor-func x-vals))))
         orders))
  
  (plot-end))

; Usage:
(plot-taylor-approximations '(sin x) 'x 0 '(1 3 5))
```

### Example 3: Critical Points Analysis

```scheme
(define (plot-critical-points expr var x-min x-max)
  (let* ((f (sym->lambda expr (list var)))
         (df-expr (sym-diff expr var))
         (df (sym->lambda df-expr (list var)))
         (critical-pts (sym-solve `(= ,df-expr 0) var))
         (x-vals (range x-min x-max 0.05)))
    
    (plot-device "pdf")
    (plot-output "critical-points.pdf")
    (plot-init)
    (plot-env x-min x-max -5.0 10.0)
    (plot-labels "x" "f(x)" "Critical Points Analysis")
    
    ; Plot function
    (plot-color 2)
    (plot-line x-vals (map f x-vals))
    
    ; Mark critical points
    (plot-color 1)
    (map (lambda (pt)
           (when (and (>= pt x-min) (<= pt x-max))
             (plot-points (list pt) (list (f pt)) 17)))
         critical-pts)
    
    (plot-end)))

; Usage:
(plot-critical-points '(+ (* x x x) (* -3 x)) 'x -3.0 3.0)
```

### Example 4: 3D Surface from Symbolic Expression

```scheme
(define (plot-symbolic-surface expr vars)
  (let* ((x-var (car vars))
         (y-var (cadr vars))
         (f (sym->lambda expr vars))
         (x-vals '(-2.0 -1.0 0.0 1.0 2.0))
         (y-vals '(-2.0 -1.0 0.0 1.0 2.0))
         (z-grid (map (lambda (y)
                       (map (lambda (x) (f x y)) x-vals))
                     y-vals)))
    
    (plot-device "pdf")
    (plot-output "symbolic-surface.pdf")
    (plot-init)
    (plot-3d-init -2.0 2.0 -2.0 2.0 -5.0 5.0 30.0 45.0)
    (plot-3d-box "X" "Y" (sym->string expr))
    (plot-color 3)
    (plot-3d-surface x-vals y-vals z-grid)
    (plot-end)))

; Usage:
(plot-symbolic-surface '(- (+ (* x x) (* y y)) 2) '(x y))
```

---

## 6. Implementation Architecture

### 6.1 File Structure

```
siod-tr/
├── symengine.c              # Module entry point
├── siod_symengine.c         # Main bindings
├── siod_symengine.h         # Header
├── symengine-utils.scm      # High-level Scheme utilities
├── tests/
│   └── test-symengine.scm   # Test suite
└── examples/
    ├── calculus-plots.scm   # Calculus visualization
    ├── taylor-series.scm    # Series expansion examples
    └── equation-solving.scm # Solving examples
```

### 6.2 Core Conversion Functions

```c
/* Convert SIOD list to SymEngine basic */
basic lisp2basic(LISP expr) {
    basic result;
    basic_new_stack(result);
    
    if (SYMBOLP(expr)) {
        symbol_set(result, get_c_string(expr));
    }
    else if (NUMBERP(expr)) {
        integer_set_si(result, get_c_long(expr));
    }
    else if (CONSP(expr)) {
        // Operator at car, operands at cdr
        LISP op = car(expr);
        LISP args = cdr(expr);
        
        if (strcmp(get_c_string(op), "+") == 0) {
            // Build addition
            basic_add(result, ...);
        }
        // ... other operators
    }
    
    return result;
}

/* Convert SymEngine basic to SIOD list */
LISP basic2lisp(basic b) {
    TypeID type = basic_get_type(b);
    
    switch(type) {
        case SYMENGINE_SYMBOL:
            return symbol_set_name(basic_str(b));
        case SYMENGINE_INTEGER:
            return flocons(integer_get_si(b));
        case SYMENGINE_ADD:
            // Reconstruct list: (+ args...)
            return cons(rintern("+"), get_args_as_lisp(b));
        // ... other types
    }
}
```

### 6.3 Module Registration

```c
/* symengine.c - Module entry point */
#include "siod.h"

extern void init_subr_symengine(void);

void init_symengine(void) {
    init_subr_symengine();
}
```

```c
/* siod_symengine.c - Primitive registration */
void init_subr_symengine(void) {
    /* Core */
    init_subr_1("sym", siod_sym);
    init_subr_1("sym-symbol", siod_sym_symbol);
    init_subr_1("sym-number", siod_sym_number);
    
    /* Algebra */
    init_subr_2("sym+", siod_sym_add);
    init_subr_2("sym-", siod_sym_sub);
    init_subr_2("sym*", siod_sym_mul);
    init_subr_2("sym/", siod_sym_div);
    init_subr_2("sym-pow", siod_sym_pow);
    
    /* Calculus */
    init_subr_2("sym-diff", siod_sym_diff);
    init_subr_4("sym-series", siod_sym_series);
    
    /* Evaluation */
    init_subr_2("sym-subs", siod_sym_subs);
    init_subr_1("sym-evalf", siod_sym_evalf);
    init_subr_2("sym->lambda", siod_sym_to_lambda);
    
    /* ... more primitives */
}
```

---

## 7. Build Integration

### 7.1 Makefile Rules

```makefile
# SymEngine detection
SYMENGINE_CFLAGS := $(shell pkg-config --cflags symengine)
SYMENGINE_LIBS := $(shell pkg-config --libs symengine)

# Build symengine.so module
symengine.so: siod_symengine.o symengine.o
	$(LD) $(LD_LIB_FLAGS) -o symengine.so siod_symengine.o symengine.o \
		$(LD_LIB_LIBS) $(SYMENGINE_LIBS)

siod_symengine.o: siod_symengine.c siod.h
	$(CC) $(CFLAGS) $(SYMENGINE_CFLAGS) -c siod_symengine.c

symengine.o: symengine.c siod.h
	$(CC) $(CFLAGS) -c symengine.c
```

### 7.2 Installation

```bash
# Ubuntu 22.04 - Build from source
sudo apt install cmake g++ libgmp-dev

git clone https://github.com/symengine/symengine
cd symengine
cmake -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DCMAKE_BUILD_TYPE=Release \
      -DWITH_SYMENGINE_THREAD_SAFE=ON \
      .
make -j4
sudo make install

# Verify
pkg-config --modversion symengine
```

---

## 8. Comparison with Alternatives

### vs. Maxima
- **SymEngine**: Faster, C API, minimal
- **Maxima**: More complete, has integration, Lisp-based
- **Verdict**: SymEngine better for SIOD (C integration, speed)

### vs. SymPy (via Python)
- **SymEngine**: Native C, 10-100x faster
- **SymPy**: Python embedding, heavier
- **Verdict**: SymEngine cleaner for SIOD integration

### vs. GiNaC
- **SymEngine**: Actively maintained, cleaner API
- **GiNaC**: Older, C++ only
- **Verdict**: SymEngine better modern choice

---

## 9. Risks and Mitigations

### Risk 1: Memory Leaks
**Probability**: Medium  
**Impact**: High  
**Mitigation**: 
- Wrapper functions that always free `basic` objects
- Valgrind testing
- Clear ownership model in documentation

### Risk 2: Integration Limitations
**Probability**: High  
**Impact**: Low  
**Mitigation**:
- Document clearly: "Integration not supported"
- Focus on differentiation, series, simplification
- Future: Could add Maxima for integration

### Risk 3: Build Complexity
**Probability**: Medium  
**Impact**: Medium  
**Mitigation**:
- Provide build scripts
- Docker container option
- Pre-built binaries for common platforms

### Risk 4: Learning Curve
**Probability**: Low  
**Impact**: Low  
**Mitigation**:
- S-expression syntax familiar to Scheme users
- Extensive examples and documentation
- Progressive disclosure (start simple)

---

## 10. Implementation Roadmap

### Phase 1: Core Infrastructure (Week 1)
- [ ] SymEngine installation and testing
- [ ] Basic C wrapper module skeleton
- [ ] `lisp2basic` and `basic2lisp` conversion
- [ ] Module loading in SIOD
- [ ] Simple arithmetic primitives

**Deliverable**: `(sym+ '(* x 2) 'x)` works

### Phase 2: Essential Operations (Week 1-2)
- [ ] All algebraic operations (+, -, *, /, ^)
- [ ] Simplification, expansion
- [ ] Differentiation
- [ ] Substitution and evaluation
- [ ] Test suite

**Deliverable**: Calculus operations functional

### Phase 3: Advanced Features (Week 2)
- [ ] Series expansion
- [ ] Special functions
- [ ] Solving (equations, linear systems)
- [ ] Matrix operations
- [ ] LaTeX output

**Deliverable**: Full symbolic toolkit

### Phase 4: PLplot Integration (Week 2-3)
- [ ] `sym->lambda` for numerical evaluation
- [ ] High-level plotting functions
- [ ] Visualization examples
- [ ] Documentation

**Deliverable**: Complete visualization pipeline

### Phase 5: Polish and Documentation (Week 3)
- [ ] Comprehensive documentation
- [ ] Example gallery
- [ ] Performance tuning
- [ ] Edge case handling

**Deliverable**: Production-ready module

---

## 11. Success Criteria

### Must Have
- ✅ Create symbolic expressions from S-expressions
- ✅ Differentiation works correctly
- ✅ Numerical evaluation for plotting
- ✅ Integration with PLplot
- ✅ No memory leaks (Valgrind clean)

### Should Have
- ✅ Series expansion
- ✅ Equation solving
- ✅ Simplification and expansion
- ✅ LaTeX output
- ✅ Comprehensive examples

### Nice to Have
- ⭐ Matrix operations
- ⭐ Special functions (Bessel, etc.)
- ⭐ Performance optimization
- ⭐ Interactive notebook integration

---

## 12. Conclusion

### Feasibility Rating: ⭐⭐⭐⭐⭐ (5/5)

SymEngine integration is **highly feasible** and **strongly recommended** for SIOD-TR:

1. **Perfect Technical Fit**
   - Clean C API designed for language bindings
   - S-expressions map naturally to symbolic trees
   - Follows same pattern as successful PLplot integration

2. **Completes the Vision**
   - Symbolic math + Numerical plotting = Complete toolkit
   - Baroque numbers + Symbolic algebra = Mathematical playground
   - Quaternions + Calculus + Visualization = Research platform

3. **Reasonable Effort**
   - 2-3 weeks estimated (similar to PLplot)
   - Well-defined scope
   - Clear implementation path

4. **High Value**
   - Transform SIOD-TR into complete mathematical environment
   - Enable research-quality mathematical work
   - Unique: symbolic Scheme + high-performance backend

### Next Steps

1. **Install SymEngine** and verify C API works
2. **Implement basic conversion** (`lisp2basic`, `basic2lisp`)
3. **Create minimal module** with arithmetic
4. **Iterate** adding features incrementally
5. **Integrate** with PLplot for visualization

### The Vision

```scheme
; Define symbolic expression
(define f '(* (exp (* -0.5 (* x x))) 
              (cos (* 3 x))))

; Differentiate it
(define df (sym-diff f 'x))

; Plot both together
(plot-with-derivative f 'x -3.0 3.0)

; Expand as Taylor series
(define taylor (sym-series f 'x 0 10))

; Visualize convergence
(plot-taylor-approximations f 'x 0 '(2 4 6 8 10))
```

**This is the future of SIOD-TR: Lisp elegance meets modern symbolic mathematics.** 🎯📐

---

**Author**: SIOD-TR Integration Study  
**Date**: January 2026  
**License**: MIT (matching SymEngine and SIOD)
