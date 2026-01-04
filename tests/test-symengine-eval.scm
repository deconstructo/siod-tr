;;; Test SymEngine Numerical Evaluation Functions
(require-so "symengine.so")

(display "=== SYMENGINE NUMERICAL EVALUATION TESTS ===") (newline) (newline)

;;; ============================================
;;; PART 1: Substitution (sym-subs)
;;; ============================================

(display "PART 1: Variable Substitution (sym-subs)") (newline)
(display "==========================================") (newline) (newline)

;;; Test 1: Simple substitution
(display "Test 1: Simple substitution") (newline)
(display "  Expression: 2*x + 3") (newline)
(display "  Substitute: x = 5") (newline)
(define expr1  '(+ (* 2 x) 3))
(display "  Result: ")
(display (sym-subs expr1 '((x . 5)))) (newline)
(display "  Expected: 13") (newline)
(newline)

;;; Test 2: Multiple variables
(display "Test 2: Multiple variable substitution") (newline)
(display "  Expression: x + y") (newline)
(display "  Substitute: x = 10, y = 20") (newline)
(define expr2  '(+ x y))
(display "  Result: ")
(display (sym-subs expr2 '((x . 10) (y . 20)))) (newline)
(display "  Expected: 30") (newline)
(newline)

;;; Test 3: Complex substitution
(display "Test 3: Substitution in complex expression") (newline)
(display "  Expression: x^2 + 2*x*y + y^2") (newline)
(display "  Substitute: x = 3, y = 4") (newline)
(define expr3  '(+ (+ (* x x) (* 2 (* x y))) (* y y)))
(display "  Result: ")
(display (sym-subs expr3 '((x . 3) (y . 4)))) (newline)
(display "  Expected: 49 (which is (3+4)^2)") (newline)
(newline)

;;; Test 4: Partial substitution
(display "Test 4: Partial substitution (leave some variables)") (newline)
(display "  Expression: x + y + z") (newline)
(display "  Substitute: x = 5, y = 10 (leave z symbolic)") (newline)
(define expr4 '(+ (+ x y) z))
(display "  Result: ")
(display (sym-subs expr4 '((x . 5) (y . 10)))) (newline)
(display "  Expected: 15 + z") (newline)
(newline)

;;; Test 5: Substitution with complex numbers
(display "Test 5: Substitute complex numbers") (newline)
(display "  Expression: x + y") (newline)
(display "  Substitute: x = 1+2i, y = 3+4i") (newline)
(define c1 (make-rectangular 1 2))
(define c2 (make-rectangular 3 4))
(define expr5  '(+ x y))
(display "  Result: ")
(display (sym-subs expr5 (list (cons 'x c1) (cons 'y c2)))) (newline)
(display "  Expected: 4.0 + 6.0*I") (newline)
(newline)

;;; ============================================
;;; PART 2: Numerical Evaluation (sym-evalf)
;;; ============================================

(display "PART 2: Numerical Evaluation (sym-evalf)") (newline)
(display "=========================================") (newline) (newline)

;;; Test 6: Evaluate pi
(display "Test 6: Evaluate pi") (newline)
(define pi-expr  'pi)
(display "  Expression: pi") (newline)
(display "  Result: ")
(display (sym-evalf pi-expr)) (newline)
(display "  Expected: ~3.14159...") (newline)
(newline)

;;; Test 7: Evaluate sqrt(2)
(display "Test 7: Evaluate sqrt(2)") (newline)
(define sqrt2-expr '(sqrt 2))
(display "  Expression: sqrt(2)") (newline)
(display "  Result: ")
(display (sym-evalf sqrt2-expr)) (newline)
(display "  Expected: ~1.41421...") (newline)
(newline)

;;; Test 8: Evaluate e^1
(display "Test 8: Evaluate e^1") (newline)
(define e-expr '(exp 1))
(display "  Expression: exp(1)") (newline)
(display "  Result: ")
(display (sym-evalf e-expr)) (newline)
(display "  Expected: ~2.71828...") (newline)
(newline)

;;; Test 9: Evaluate sin(pi/2)
(display "Test 9: Evaluate sin(pi/2)") (newline)
(define sin-expr '(sin (/ pi 2)))
(display "  Expression: sin(pi/2)") (newline)
(display "  Result: ")
(display (sym-evalf sin-expr)) (newline)
(display "  Expected: 1.0") (newline)
(newline)

;;; ============================================
;;; PART 3: Combined Workflow
;;; ============================================

(display "PART 3: Combined Substitution + Evaluation") (newline)
(display "===========================================") (newline) (newline)

;;; Test 10: Substitute then evaluate
(display "Test 10: Substitute variables, then evaluate numerically") (newline)
(display "  Expression: sqrt(x^2 + y^2)") (newline)
(display "  Substitute: x = 3, y = 4") (newline)
(display "  Then evaluate numerically") (newline)
(define dist-expr  '(sqrt (+ (* x x) (* y y))))
(define dist-subs (sym-subs dist-expr '((x . 3) (y . 4))))
(display "  After substitution: ")
(display dist-subs) (newline)
(display "  After evaluation: ")
(display (sym-evalf dist-subs)) (newline)
(display "  Expected: 5.0") (newline)
(newline)

;;; Test 11: Symbolic simplification then evaluation
(display "Test 11: Expand, substitute, evaluate") (newline)
(display "  Expression: (x + 1)^2") (newline)
(display "  Expand: x^2 + 2*x + 1") (newline)
(display "  Substitute: x = 10") (newline)
(display "  Evaluate: 121") (newline)
(define poly-expr  '(pow (+ x 1) 2))
(define poly-expanded (sym-expand poly-expr))
(display "  Expanded: ")
(display poly-expanded) (newline)
(define poly-subs (sym-subs poly-expanded '((x . 10))))
(display "  After substitution: ")
(display poly-subs) (newline)
(display "  After evaluation: ")
(display (sym-evalf poly-subs)) (newline)
(display "  Expected: 121.0") (newline)
(newline)

;;; ============================================
;;; SUMMARY
;;; ============================================

(display "=== SUMMARY ===") (newline) (newline)

(display "New SymEngine Functions:") (newline)
(display "  ✓ (sym-subs expr bindings) - Substitute variables") (newline)
(display "    bindings: '((var1 . val1) (var2 . val2) ...)") (newline)
(newline)
(display "  ✓ (sym-evalf expr) - Evaluate to floating point") (newline)
(display "    Returns: LISP float for real results") (newline)
(display "    Returns: String for complex results") (newline)
(newline)

(display "Typical Workflow:") (newline)
(display "  1. Create symbolic expression:  '(+ (* a x) b)") (newline)
(display "  2. Manipulate symbolically: (sym-diff ...), (sym-expand ...)") (newline)
(display "  3. Substitute values: (sym-subs expr '((x . 5) (y . 10)))") (newline)
(display "  4. Evaluate numerically: (sym-evalf expr)") (newline)
(newline)

(display "This enables full symbolic-to-numeric workflow!") (newline)
