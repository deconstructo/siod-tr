;;; Test sym-simplify
(require-so "symengine.so")

(display "=== SYM-SIMPLIFY TEST ===") (newline) (newline)

;;; Test 1: Expand (x+1)(x-1)
(display "Test 1: Expand product") (newline)
(display "  Expression: (x+1)(x-1)") (newline)
(display "  Simplified: ")
(display (sym-simplify '(* (+ x 1) (- x 1)))) (newline)
(display "  Expected: x**2 - 1") (newline)
(newline)

;;; Test 2: Trigonometric identity
(display "Test 2: Trig identity") (newline)
(display "  Expression: sin^2(x) + cos^2(x)") (newline)
(display "  Simplified: ")
(display (sym-simplify '(+ (* (sin x) (sin x)) (* (cos x) (cos x))))) (newline)
(display "  Expected: 1 (if SymEngine knows trig identities)") (newline)
(newline)

;;; Test 3: Algebraic simplification
(display "Test 3: Algebraic") (newline)
(display "  Expression: (x^2 - 1)/(x - 1)") (newline)
(display "  Simplified: ")
(display (sym-simplify '(/ (- (* x x) 1) (- x 1)))) (newline)
(display "  Expected: x + 1 (if SymEngine does factoring)") (newline)
(newline)

;;; Test 4: Numeric
(display "Test 4: Numeric expression") (newline)
(display "  Expression: 2 + 3 * 4") (newline)
(display "  Simplified: ")
(define result (sym-simplify '(+ 2 (* 3 4))))
(display result)
(display " (type: ")
(display (if (number? result) "NUMBER" "STRING"))
(display ")") (newline)
(display "  Expected: 14 (as number)") (newline)
(newline)

(display "Note: SymEngine's 'simplify' may just be 'expand' for now.") (newline)
(display "We'll enhance it once we explore the API more.") (newline)
