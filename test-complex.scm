;;; test-complex.scm - Complex Numbers Test Suite
;;; SIOD-TR Baroque Numbers Phase 1
;;;
;;; Usage: (load "test-complex.scm")
;;;        (run-all-tests)

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test framework
(define (test-equal name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "✓ ")
        (display name)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "✗ ")
        (display name)
        (newline)
        (display "  Expected: ")
        (display expected)
        (newline)
        (display "  Got:      ")
        (display actual)
        (newline))))

(define (test-approx name expected actual epsilon)
  (set! test-count (+ test-count 1))
  (let ((diff (abs (- expected actual))))
    (if (< diff epsilon)
        (begin
          (set! pass-count (+ pass-count 1))
          (display "✓ ")
          (display name)
          (newline))
        (begin
          (set! fail-count (+ fail-count 1))
          (display "✗ ")
          (display name)
          (newline)
          (display "  Expected: ~")
          (display expected)
          (display " (±")
          (display epsilon)
          (display ")")
          (newline)
          (display "  Got:      ")
          (display actual)
          (newline)))))

(define (section title)
  (newline)
  (display "=== ")
  (display title)
  (display " ===")
  (newline))

;;; Test Suite
(define (test-constructors)
  (section "Constructors")
  
  (define z1 (make-rectangular 3 4))
  (test-equal "make-rectangular returns complex"
              #t
              (complex? z1))
  
  (test-approx "make-rectangular real part"
               3.0 (real-part z1) 0.001)
  
  (test-approx "make-rectangular imag part"
               4.0 (imag-part z1) 0.001)
  
  (define z2 (make-polar 5 0.9273))
  (test-equal "make-polar returns complex"
              #t (complex? z2))
  
  (test-approx "make-polar creates correct real"
               3.0 (real-part z2) 0.01)
  
  (test-approx "make-polar creates correct imag"
               4.0 (imag-part z2) 0.01))

(define (test-accessors)
  (section "Accessors")
  
  (define z (make-rectangular 3 4))
  (test-approx "real-part" 3.0 (real-part z) 0.001)
  (test-approx "imag-part" 4.0 (imag-part z) 0.001)
  (test-approx "magnitude" 5.0 (magnitude z) 0.001)
  (test-approx "angle" 0.927 (angle z) 0.01)
  
  (test-approx "real-part of real" 42.0 (real-part 42) 0.001)
  (test-approx "imag-part of real" 0.0 (imag-part 42.0) 0.001)
  (test-approx "magnitude of real" 5.0 (magnitude 5.0) 0.001)
  (test-approx "angle of -1 is π" 3.14159 (angle -1.0) 0.01))

(define (test-polar)
  (section "Polar Conversion")
  
  (define z (make-rectangular 3 4))
  (define mag (magnitude z))
  (define ang (angle z))
  (define z2 (make-polar mag ang))
  
  (test-approx "round-trip real" (real-part z) (real-part z2) 0.01)
  (test-approx "round-trip imag" (imag-part z) (imag-part z2) 0.01)
  
  (define i (make-polar 1 1.5708))
  (test-approx "i real ≈ 0" 0.0 (real-part i) 0.01)
  (test-approx "i imag ≈ 1" 1.0 (imag-part i) 0.01))

(define (test-predicates)
  (section "Predicates")
  
  (define z (make-rectangular 3 4))
  (test-equal "complex? on complex" #t (complex? z))
  (test-equal "complex? on real" #f (complex? 42))
  (test-equal "complex? on string" #f (complex? "hello")))

(define (test-printing)
  (section "Printing")
  (display "Visual: ")
  (display (make-rectangular 3 4))
  (display " (expect #C(3 4))")
  (newline))

(define (run-all-tests)
  (set! test-count 0)
  (set! pass-count 0)
  (set! fail-count 0)
  
  (display "SIOD-TR Complex Numbers - Test Suite")
  (newline)
  (display "====================================")
  (newline)
  
  (test-constructors)
  (test-accessors)
  (test-polar)
  (test-predicates)
  (test-printing)
  
  (newline)
  (display "====================================")
  (newline)
  (display "Results: ")
  (display pass-count)
  (display "/")
  (display test-count)
  (display " passed")
  (newline)
  
  (if (> fail-count 0)
      (display "FAILED")
      (display "SUCCESS"))
  (newline))

(display "Test suite loaded. Run: (run-all-tests)")
(newline)
