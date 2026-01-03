# SymEngine for SIOD-TR - API Quick Reference

## Recommended Scheme Interface

This document shows the **recommended API** for using SymEngine from SIOD-TR, designed for maximum usability and natural Scheme style.

---

## Philosophy

1. **S-expressions ARE symbolic expressions** - No separate syntax to learn
2. **Functional style** - Pure functions, no side effects
3. **Natural integration** - Works seamlessly with PLplot and other modules
4. **Progressive disclosure** - Simple things simple, complex things possible

---

## Basic Usage Pattern

```scheme
; Load module
(require-so "symengine.so")

; Create symbolic expression
(define expr '(+ (* x x) (* 2 x) 1))

; Perform operations
(sym-diff expr 'x)              ; Differentiate
(sym-simplify expr)             ; Simplify
(sym-subs expr '((x . 5)))      ; Evaluate at x=5

; Convert to plottable function
(define f (sym->lambda expr '(x)))
(plot-line (range 0 10 0.1) (map f (range 0 10 0.1)))
```

---

## Core Functions

### Expression Creation

```scheme
(sym 'x)                        ; Symbol
(sym '(+ x 1))                  ; Expression from S-expr
(sym 5)                         ; Number
(sym 'pi)                       ; Constant

(sym-symbol "theta")            ; Create symbol explicitly
(sym-number 42)                 ; Create number explicitly
```

### Arithmetic

```scheme
(sym+ '(* x 2) '(* x 3))        ; => 5*x
(sym- 'x 1)                     ; => x - 1
(sym* 'x 'y)                    ; => x*y
(sym/ 'x 2)                     ; => x/2
(sym-pow 'x 2)                  ; => x^2
(sym-sqrt 'x)                   ; => sqrt(x)
```

### Calculus

```scheme
; Differentiation
(sym-diff '(* x x) 'x)                    ; => 2*x
(sym-diff '(sin x) 'x)                    ; => cos(x)
(sym-diff-n '(* x x x x) 'x 2)            ; 2nd derivative

; Series expansion
(sym-series '(exp x) 'x 0 5)              ; Taylor series at x=0, order 5
(sym-series '(sin x) 'x 0 7)              ; sin(x) to 7th order
```

### Algebraic Manipulation

```scheme
(sym-simplify '(+ (* x 0) (* x 1)))       ; => x
(sym-expand '(* (+ x 1) (+ x 2)))         ; => x^2 + 3*x + 2
(sym-factor '(+ (* x x) (* 2 x) 1))       ; => (x + 1)^2
```

### Evaluation

```scheme
; Substitution
(sym-subs '(+ (* x x) 1) '((x . 5)))      ; => 26

; Multiple substitutions
(sym-subs '(* x y) '((x . 2) (y . 3)))    ; => 6

; Numerical evaluation
(sym-evalf '(sin 1))                      ; => 0.8414709...

; Convert to Scheme function
(define f (sym->lambda '(* x x) '(x)))
(f 5)                                      ; => 25
(map f '(1 2 3 4 5))                      ; => (1 4 9 16 25)
```

### Special Functions

```scheme
; Trig
(sym '(sin x))                  ; sin(x)
(sym '(cos x))                  ; cos(x)
(sym '(tan x))                  ; tan(x)

; Exponential/Log
(sym '(exp x))                  ; e^x
(sym '(log x))                  ; ln(x)

; Constants
(sym 'pi)                       ; π
(sym 'e)                        ; e
(sym 'i)                        ; √-1
```

### Output

```scheme
(sym->string '(/ (+ x 1) (- x 1)))        ; => "(x + 1)/(x - 1)"
(sym-display expr)                        ; Pretty print to stdout
(sym->latex '(/ x 2))                     ; => "\\frac{x}{2}"
```

---

## Integration with PLplot

### Pattern 1: Direct Plotting

```scheme
(define (plot-symbolic expr var x-min x-max)
  (let* ((f (sym->lambda expr (list var)))
         (x-vals (range x-min x-max 0.1))
         (y-vals (map f x-vals)))
    
    (plot-device "pdf")
    (plot-output "plot.pdf")
    (plot-init)
    (plot-env x-min x-max 
              (apply min y-vals) 
              (apply max y-vals))
    (plot-labels (symbol->string var) 
                 (sym->string expr) 
                 "Symbolic Function")
    (plot-line x-vals y-vals)
    (plot-end)))

; Usage:
(plot-symbolic '(sin (* 2 x)) 'x 0 6.28)
```

### Pattern 2: Function and Derivative

```scheme
(define (plot-with-derivative expr var x-min x-max)
  (let* ((f (sym->lambda expr (list var)))
         (df (sym->lambda (sym-diff expr var) (list var)))
         (x-vals (range x-min x-max 0.05)))
    
    (plot-device "pdf")
    (plot-output "function-derivative.pdf")
    (plot-init)
    (plot-env x-min x-max -5 5)
    (plot-labels "x" "y" 
                 (string-append (sym->string expr) " and its derivative"))
    
    ; Plot function
    (plot-color 2)
    (plot-line x-vals (map f x-vals))
    
    ; Plot derivative
    (plot-color 4)
    (plot-line x-vals (map df x-vals))
    
    (plot-end)))

; Usage:
(plot-with-derivative '(* x x x) 'x -2 2)
```

### Pattern 3: Taylor Series Approximations

```scheme
(define (plot-taylor expr var center max-order)
  (plot-device "pdf")
  (plot-output "taylor.pdf")
  (plot-init)
  (plot-subplot 2 2)
  
  (let ((f (sym->lambda expr (list var)))
        (x-vals (range -3 3 0.05)))
    
    (map (lambda (order)
           (let ((taylor-expr (sym-series expr var center order)))
             (let ((taylor-f (sym->lambda taylor-expr (list var))))
               
               (plot-advance)
               (plot-env -3 3 -2 2)
               (plot-labels "x" "y" 
                           (string-append "Order " (number->string order)))
               
               ; Original function
               (plot-color 1)
               (plot-line x-vals (map f x-vals))
               
               ; Taylor approximation
               (plot-color 2)
               (plot-line x-vals (map taylor-f x-vals)))))
         (list 2 4 6 8)))
  
  (plot-end))

; Usage:
(plot-taylor '(sin x) 'x 0 8)
```

### Pattern 4: Surface from Symbolic Expression

```scheme
(define (plot-symbolic-surface expr vars)
  (let* ((x-var (car vars))
         (y-var (cadr vars))
         (f (sym->lambda expr vars))
         (x-vals '(-2 -1 0 1 2))
         (y-vals '(-2 -1 0 1 2))
         (z-grid (map (lambda (y)
                       (map (lambda (x) (f x y)) x-vals))
                     y-vals)))
    
    (plot-device "pdf")
    (plot-output "surface.pdf")
    (plot-init)
    (plot-3d-init -2 2 -2 2 -5 5 30 45)
    (plot-3d-box "X" "Y" (sym->string expr))
    (plot-3d-surface x-vals y-vals z-grid)
    (plot-end)))

; Usage:
(plot-symbolic-surface '(- (+ (* x x) (* y y)) 2) '(x y))
```

---

## Common Workflows

### Workflow 1: Explore Function Behavior

```scheme
; Define function
(define f '(/ (* x x x) (+ (* x x) 1)))

; Find derivative
(define df (sym-diff f 'x))

; Simplify derivative
(define df-simple (sym-simplify df))

; Find critical points
(define critical (sym-solve `(= ,df-simple 0) 'x))

; Plot function with critical points marked
(plot-function-with-critical-points f 'x critical -3 3)
```

### Workflow 2: Compare Approximations

```scheme
; Original function
(define f '(exp x))

; Generate Taylor series of different orders
(define taylor-2 (sym-series f 'x 0 2))
(define taylor-4 (sym-series f 'x 0 4))
(define taylor-6 (sym-series f 'x 0 6))

; Plot all on same graph
(plot-multiple-functions 
  (list f taylor-2 taylor-4 taylor-6)
  '(x)
  -2 2
  '("Original" "Order 2" "Order 4" "Order 6"))
```

### Workflow 3: Mandelbrot Analysis (Symbolic)

```scheme
; Define Mandelbrot iteration symbolically
(define mandel-iter '(+ (* z z) c))

; Compute first few iterations
(define z1 (sym-subs mandel-iter '((z . 0))))  ; First iteration
(define z2 (sym-subs mandel-iter `((z . ,z1)))) ; Second iteration
(define z3 (sym-subs mandel-iter `((z . ,z2)))) ; Third iteration

; Analyze derivative for stability
(define dz3-dc (sym-diff z3 'c))
(sym-simplify dz3-dc)  ; Simplified form

; Plot magnitude as function of c
(plot-symbolic '(abs dz3-dc) 'c -2 2)
```

---

## Advanced: Creating Custom Operations

### Example: Gradient

```scheme
(define (sym-gradient expr vars)
  (map (lambda (var) (sym-diff expr var)) vars))

; Usage:
(sym-gradient '(+ (* x x) (* y y)) '(x y))
; => ((* 2 x) (* 2 y))
```

### Example: Jacobian Matrix

```scheme
(define (sym-jacobian exprs vars)
  (map (lambda (expr)
         (map (lambda (var)
                (sym-diff expr var))
              vars))
       exprs))

; Usage:
(sym-jacobian '((* x y) (+ x y)) '(x y))
; => ((y x) (1 1))
```

### Example: Numerical Integration (Trapezoid Rule)

```scheme
(define (sym-integrate-numerical expr var a b n)
  (let* ((f (sym->lambda expr (list var)))
         (h (/ (- b a) n))
         (x-vals (range a b h)))
    (* h (+ (* 0.5 (+ (f a) (f b)))
            (apply + (map f (cdr (reverse (cdr x-vals)))))))))

; Usage:
(sym-integrate-numerical '(* x x) 'x 0 1 1000)
; => ~0.333 (exact: 1/3)
```

---

## Error Handling

All SymEngine functions should return proper error values:

```scheme
; Invalid expression
(sym '(this is not valid))
; => ERROR: invalid symbolic expression

; Division by zero
(sym-subs '(/ x y) '((y . 0)))
; => ERROR: division by zero

; Undefined variable
(sym-diff '(+ x y) 'z)
; => 0  (derivative of expression w.r.t. absent variable is 0)
```

---

## Performance Tips

1. **Compile once, use many times**
   ```scheme
   (define f (sym->lambda expr '(x)))  ; Compile once
   (map f (range 0 100 0.1))           ; Use many times
   ```

2. **Simplify before evaluation**
   ```scheme
   (define expr-simple (sym-simplify complex-expr))
   (define f (sym->lambda expr-simple '(x)))
   ```

3. **Use substitution for multiple variables**
   ```scheme
   ; Better:
   (sym-subs expr '((x . 1) (y . 2) (z . 3)))
   
   ; Worse:
   (sym-subs (sym-subs (sym-subs expr '((x . 1))) '((y . 2))) '((z . 3)))
   ```

---

## Summary

### Most Common Operations

```scheme
; 1. Create expression
(define f '(* (sin x) (exp (* -0.1 x))))

; 2. Differentiate
(define df (sym-diff f 'x))

; 3. Convert to function
(define fn (sym->lambda f '(x)))

; 4. Plot
(plot-function-symbolic f 'x 0 10)

; 5. Evaluate at specific point
(sym-subs f '((x . 3.14)))
```

### Key Design Decisions

1. **S-expressions for symbolic math** - Natural for Scheme
2. **Separate namespace** (`sym-*`) - Avoid conflicts
3. **Lists for substitutions** - `'((x . 1) (y . 2))`
4. **Return symbolic expressions** - Keep working symbolically
5. **`sym->lambda` for numerical** - Bridge to computation

This API makes SIOD-TR a powerful symbolic mathematics environment while maintaining Scheme's elegance! 🎯
