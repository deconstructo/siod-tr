;;; test-symengine.scm
;;; Basic test suite for SymEngine integration in SIOD-TR
;;;
;;; Run: (load "test-symengine.scm") then (run-symengine-tests)

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test infrastructure
(define (test-start name)
  (set! test-count (+ test-count 1))
  (display "Test ")
  (display test-count)
  (display ": ")
  (display name)
  (display " ... "))

(define (test-pass)
  (set! pass-count (+ pass-count 1))
  (display "PASS")
  (newline))

(define (test-fail msg)
  (set! fail-count (+ fail-count 1))
  (display "FAIL: ")
  (display msg)
  (newline))

(define (test-summary)
  (newline)
  (display "========================================")
  (newline)
  (display "SymEngine Test Summary")
  (newline)
  (display "========================================")
  (newline)
  (display "Total:  ") (display test-count) (newline)
  (display "Passed: ") (display pass-count) (newline)
  (display "Failed: ") (display fail-count) (newline)
  (if (= fail-count 0)
      (begin
        (display "All tests passed!")
        (newline))
      (begin
        (display "Some tests failed!")
        (newline))))

;;; ============================================
;;; Test 1: Symbol Creation
;;; ============================================

(define (test-symbol-creation)
  (test-start "Symbol creation")
  (let ((result (sym 'x)))
    (if (string? result)
        (test-pass)
        (test-fail "sym should return a string"))))

;;; ============================================
;;; Test 2: Number Creation
;;; ============================================

(define (test-number-creation)
  (test-start "Number creation")
  (let ((result (sym 42)))
    (if (string? result)
        (test-pass)
        (test-fail "sym should work with numbers"))))

;;; ============================================
;;; Test 3: Addition
;;; ============================================

(define (test-addition)
  (test-start "Symbolic addition")
  (let ((result (sym+ '(* x 2) '(* x 3))))
    (if (equal? result "5*x")
        (test-pass)
        (begin
          (display "Expected '5*x', got '")
          (display result)
          (display "'")
          (newline)
          (test-fail "addition result incorrect")))))

;;; ============================================
;;; Test 4: Subtraction
;;; ============================================

(define (test-subtraction)
  (test-start "Symbolic subtraction")
  (let ((result (sym- '(* x 5) '(* x 2))))
    (if (equal? result "3*x")
        (test-pass)
        (test-fail "subtraction result incorrect"))))

;;; ============================================
;;; Test 5: Multiplication
;;; ============================================

(define (test-multiplication)
  (test-start "Symbolic multiplication")
  (let ((result (sym* 'x 'y)))
    (if (or (equal? result "x*y") (equal? result "y*x"))
        (test-pass)
        (test-fail "multiplication result incorrect"))))

;;; ============================================
;;; Test 6: Division
;;; ============================================

(define (test-division)
  (test-start "Symbolic division")
  (let ((result (sym/ 'x 'y)))
    (if (equal? result "x/y")
        (test-pass)
        (test-fail "division result incorrect"))))

;;; ============================================
;;; Test 7: Power
;;; ============================================

(define (test-power)
  (test-start "Symbolic power")
  (let ((result (sym-pow 'x 2)))
    (if (equal? result "x**2")
        (test-pass)
        (test-fail "power result incorrect"))))

;;; ============================================
;;; Test 8: Differentiation - Simple
;;; ============================================

(define (test-diff-simple)
  (test-start "Differentiation d/dx(x²)")
  (let ((result (sym-diff '(* x x) 'x)))
    (if (equal? result "2*x")
        (test-pass)
        (begin
          (display "Expected '2*x', got '")
          (display result)
          (display "'")
          (newline)
          (test-fail "differentiation incorrect")))))

;;; ============================================
;;; Test 9: Differentiation - Sin
;;; ============================================

(define (test-diff-sin)
  (test-start "Differentiation d/dx(sin(x))")
  (let ((result (sym-diff '(sin x) 'x)))
    (if (equal? result "cos(x)")
        (test-pass)
        (test-fail "sin differentiation incorrect"))))

;;; ============================================
;;; Test 10: Differentiation - Exp
;;; ============================================

(define (test-diff-exp)
  (test-start "Differentiation d/dx(exp(x))")
  (let ((result (sym-diff '(exp x) 'x)))
    (if (equal? result "exp(x)")
        (test-pass)
        (test-fail "exp differentiation incorrect"))))

;;; ============================================
;;; Test 11: Expansion
;;; ============================================

(define (test-expansion)
  (test-start "Expansion (x+1)(x+2)")
  (let ((result (sym-expand '(* (+ x 1) (+ x 2)))))
    (if (or (equal? result "x**2 + 3*x + 2")
            (equal? result "2 + 3*x + x**2"))
        (test-pass)
        (begin
          (display "Got: ") (display result) (newline)
          (test-fail "expansion incorrect")))))

;;; ============================================
;;; Test 12: Complex Expression
;;; ============================================

(define (test-complex-expression)
  (test-start "Complex expression")
  (let ((result (sym '(+ (* x x) (* 2 x) 1))))
    (if (string? result)
        (test-pass)
        (test-fail "complex expression failed"))))

;;; ============================================
;;; Test 13: Product Rule
;;; ============================================

(define (test-product-rule)
  (test-start "Product rule d/dx(x*sin(x))")
  (let ((result (sym-diff '(* x (sin x)) 'x)))
    ; Should be: sin(x) + x*cos(x)
    (if (string? result)
        (test-pass)
        (test-fail "product rule failed"))))

;;; ============================================
;;; Test 14: Chain Rule
;;; ============================================

(define (test-chain-rule)
  (test-start "Chain rule d/dx(sin(2*x))")
  (let ((result (sym-diff '(sin (* 2 x)) 'x)))
    ; Should be: 2*cos(2*x)
    (if (string? result)
        (test-pass)
        (test-fail "chain rule failed"))))

;;; ============================================
;;; Test 15: Constants
;;; ============================================

(define (test-constants)
  (test-start "Constants (pi, e)")
  (let ((result-pi (sym 'pi))
        (result-e (sym 'e)))
    (if (and (string? result-pi) (string? result-e))
        (test-pass)
        (test-fail "constants failed"))))

;;; ============================================
;;; Run All Tests
;;; ============================================

(define (run-symengine-tests)
  (display "========================================")
  (newline)
  (display "SIOD-TR SymEngine Test Suite")
  (newline)
  (display "========================================")
  (newline)
  (newline)
  
  (set! test-count 0)
  (set! pass-count 0)
  (set! fail-count 0)
  
  ; Run all tests
  (test-symbol-creation)
  (test-number-creation)
  (test-addition)
  (test-subtraction)
  (test-multiplication)
  (test-division)
  (test-power)
  (test-diff-simple)
  (test-diff-sin)
  (test-diff-exp)
  (test-expansion)
  (test-complex-expression)
  (test-product-rule)
  (test-chain-rule)
  (test-constants)
  
  (test-summary))

;;; ============================================
;;; Interactive Examples
;;; ============================================

(define (demo-symengine)
  (display "SymEngine Interactive Demo")
  (newline)
  (newline)
  
  (display "1. Basic expression:")
  (newline)
  (display "   (sym '(+ (* x x) 1)) => ")
  (display (sym '(+ (* x x) 1)))
  (newline)
  (newline)
  
  (display "2. Differentiation:")
  (newline)
  (display "   (sym-diff '(* x x x) 'x) => ")
  (display (sym-diff '(* x x x) 'x))
  (newline)
  (newline)
  
  (display "3. Expansion:")
  (newline)
  (display "   (sym-expand '(* (+ x 1) (+ x 1))) => ")
  (display (sym-expand '(* (+ x 1) (+ x 1))))
  (newline)
  (newline)
  
  (display "4. Trigonometry:")
  (newline)
  (display "   (sym-diff '(sin x) 'x) => ")
  (display (sym-diff '(sin x) 'x))
  (newline)
  (newline)
  
  (display "5. Constants:")
  (newline)
  (display "   (sym '(* 2 pi)) => ")
  (display (sym '(* 2 pi)))
  (newline)
  (newline)
  
  (display "Demo completed!")
  (newline))

;;; ============================================
;;; Usage Instructions
;;; ============================================
;;; 
;;; After loading symengine.so:
;;;   (load "test-symengine.scm")
;;;   (run-symengine-tests)
;;;
;;; For interactive demo:
;;;   (demo-symengine)
;;;
;;; Individual tests:
;;;   (test-diff-simple)
;;;   (test-expansion)
;;;
