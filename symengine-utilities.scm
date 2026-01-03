;;; symengine-utils.scm
;;; Utility functions for working with SymEngine in SIOD-TR
;;;
;;; Load after symengine.so: (load "symengine-utils.scm")

;;; ============================================
;;; Display Helpers
;;; ============================================

;;; Pretty-print symbolic expression
(define (sym-display expr)
  (display (sym->string expr))
  (newline))

;;; Display expression with label
(define (sym-show label expr)
  (display label)
  (display ": ")
  (sym-display expr))

;;; ============================================
;;; Common Mathematical Operations
;;; ============================================

;;; Square root
(define (sym-sqrt expr)
  (sym '(sqrt ,expr)))

;;; Absolute value
(define (sym-abs expr)
  (sym '(abs ,expr)))

;;; Sine
(define (sym-sin expr)
  (sym '(sin ,expr)))

;;; Cosine
(define (sym-cos expr)
  (sym '(cos ,expr)))

;;; Tangent
(define (sym-tan expr)
  (sym '(tan ,expr)))

;;; Exponential
(define (sym-exp expr)
  (sym '(exp ,expr)))

;;; Natural logarithm
(define (sym-log expr)
  (sym '(log ,expr)))

;;; ============================================
;;; Higher-Order Derivatives
;;; ============================================

;;; Compute nth derivative
(define (sym-diff-n expr var n)
  (if (<= n 0)
      expr
      (sym-diff-n (sym-diff expr var) var (- n 1))))

;;; Second derivative
(define (sym-diff-2 expr var)
  (sym-diff-n expr var 2))

;;; Third derivative
(define (sym-diff-3 expr var)
  (sym-diff-n expr var 3))

;;; ============================================
;;; Expression Analysis
;;; ============================================

;;; Check if expression contains a variable
(define (sym-has-var? expr var)
  ; Try differentiating; if result is "0", variable not present
  (not (equal? (sym-diff expr var) "0")))

;;; ============================================
;;; Common Expression Patterns
;;; ============================================

;;; Polynomial: a*x^n + b*x^(n-1) + ... + c
(define (sym-polynomial coeffs var)
  (define (build-term coef power)
    (if (= power 0)
        coef
        `(* ,coef (^ ,var ,power))))
  
  (let ((n (length coeffs)))
    (let loop ((cs coeffs) (power (- n 1)) (result 0))
      (if (null? cs)
          result
          (loop (cdr cs)
                (- power 1)
                `(+ ,(build-term (car cs) power) ,result))))))

;;; Quadratic: ax² + bx + c
(define (sym-quadratic a b c var)
  `(+ (* ,a (^ ,var 2)) (* ,b ,var) ,c))

;;; Linear: mx + b
(define (sym-linear m b var)
  `(+ (* ,m ,var) ,b))

;;; ============================================
;;; Calculus Helpers
;;; ============================================

;;; Find critical points (where derivative = 0)
;;; Returns the derivative (user must solve manually for now)
(define (sym-critical-points expr var)
  (sym-diff expr var))

;;; Tangent line at point x0: f(x0) + f'(x0)*(x - x0)
(define (sym-tangent-line expr var x0)
  (let ((df (sym-diff expr var)))
    `(+ (,expr (,var . ,x0))
        (* ,df (- ,var ,x0)))))

;;; ============================================
;;; Expression Transformations
;;; ============================================

;;; Compose two expressions: f(g(x))
(define (sym-compose f g var)
  ; This is tricky without proper substitution
  ; Placeholder for now
  `(,f (,g ,var)))

;;; ============================================
;;; Display Utilities
;;; ============================================

;;; Display derivative nicely
(define (show-derivative expr var)
  (display "f(")
  (display var)
  (display ") = ")
  (sym-display expr)
  (display "f'(")
  (display var)
  (display ") = ")
  (sym-display (sym-diff expr var)))

;;; Display first n derivatives
(define (show-derivatives expr var n)
  (let loop ((i 0) (current expr))
    (when (<= i n)
      (display "f")
      (when (> i 0)
        (display "(")
        (display i)
        (display ")"))
      (display "(")
      (display var)
      (display ") = ")
      (sym-display current)
      (loop (+ i 1) (sym-diff current var)))))

;;; ============================================
;;; Common Mathematical Expressions
;;; ============================================

;;; Gaussian function
(define (sym-gaussian mean stddev var)
  `(* (/ 1 (* ,stddev (sqrt (* 2 pi))))
      (exp (* -0.5 (^ (/ (- ,var ,mean) ,stddev) 2)))))

;;; Sigmoid function
(define (sym-sigmoid var)
  `(/ 1 (+ 1 (exp (* -1 ,var)))))

;;; Logistic growth
(define (sym-logistic K r t)
  `(/ ,K (+ 1 (* (- (/ ,K P0) 1) (exp (* -1 ,r ,t))))))

;;; ============================================
;;; Example Usage Functions
;;; ============================================

(define (example-basic)
  (display "=== Basic SymEngine Examples ===")
  (newline)
  (newline)
  
  (display "1. Create expression:")
  (newline)
  (sym-show "  f(x)" '(+ (* x x) (* 2 x) 1))
  (newline)
  
  (display "2. Differentiate:")
  (newline)
  (sym-show "  f'(x)" (sym-diff '(+ (* x x) (* 2 x) 1) 'x))
  (newline)
  
  (display "3. Expand:")
  (newline)
  (sym-show "  expanded" (sym-expand '(* (+ x 1) (+ x 2))))
  (newline))

(define (example-calculus)
  (display "=== Calculus Examples ===")
  (newline)
  (newline)
  
  (display "Product rule: d/dx[x·sin(x)]")
  (newline)
  (sym-show "  " (sym-diff '(* x (sin x)) 'x))
  (newline)
  
  (display "Chain rule: d/dx[sin(x²)]")
  (newline)
  (sym-show "  " (sym-diff '(sin (* x x)) 'x))
  (newline)
  
  (display "Second derivative: d²/dx²[x⁴]")
  (newline)
  (sym-show "  " (sym-diff-2 '(* x (* x (* x x))) 'x))
  (newline))

(define (example-trig)
  (display "=== Trigonometric Examples ===")
  (newline)
  (newline)
  
  (display "d/dx[sin(x)]:")
  (newline)
  (sym-show "  " (sym-diff '(sin x) 'x))
  (newline)
  
  (display "d/dx[cos(x)]:")
  (newline)
  (sym-show "  " (sym-diff '(cos x) 'x))
  (newline)
  
  (display "d/dx[tan(x)]:")
  (newline)
  (sym-show "  " (sym-diff '(tan x) 'x))
  (newline))

;;; ============================================
;;; Utilities Loaded Message
;;; ============================================

(display "SymEngine utilities loaded.")
(newline)
(display "Try: (example-basic), (example-calculus), (example-trig)")
(newline)
