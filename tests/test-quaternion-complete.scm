;;; ============================================
;;; SIOD-TR QUATERNION COMPLETE TEST SUITE
;;; Tests ALL quaternion functionality
;;; ============================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal name actual expected tolerance)
  (set! test-count (+ test-count 1))
  (display test-count) (display ". ") (display name) (display ": ")
  (let ((diff (if (number? actual)
                  (abs (- actual expected))
                  (if (quaternion? actual)
                      (magnitude (- actual expected))
                      999999))))
    (if (< diff tolerance)
        (begin
          (display "✓ PASS")
          (set! pass-count (+ pass-count 1)))
        (begin
          (display "✗ FAIL (got ")
          (display actual)
          (display ", expected ")
          (display expected)
          (display ")")
          (set! fail-count (+ fail-count 1))))
    (newline)))

(define (assert-close name actual expected)
  (assert-equal name actual expected 1e-10))

(define (section title)
  (newline)
  (display "=== ") (display title) (display " ===")
  (newline) (newline))

(display "╔════════════════════════════════════════════╗") (newline)
(display "║  SIOD-TR QUATERNION COMPLETE TEST SUITE   ║") (newline)
(display "╚════════════════════════════════════════════╝") (newline)

;;; ============================================
;;; SECTION 1: CONSTRUCTION & ACCESSORS
;;; ============================================

(section "CONSTRUCTION & ACCESSORS")

(define q1 (quat 1 2 3 4))
(assert-equal "quat-w" (quat-w q1) 1 1e-15)
(assert-equal "quat-x" (quat-x q1) 2 1e-15)
(assert-equal "quat-y" (quat-y q1) 3 1e-15)
(assert-equal "quat-z" (quat-z q1) 4 1e-15)

(define i (quat 0 1 0 0))
(define j (quat 0 0 1 0))
(define k (quat 0 0 0 1))

;;; ============================================
;;; SECTION 2: HAMILTON'S RELATIONS
;;; ============================================

(section "HAMILTON'S FUNDAMENTAL RELATIONS")

(assert-equal "i² = -1" (* i i) -1 1e-15)
(assert-equal "j² = -1" (* j j) -1 1e-15)
(assert-equal "k² = -1" (* k k) -1 1e-15)
(assert-equal "ijk = -1" (* i (* j k)) -1 1e-15)

(assert-equal "ij = k" (* i j) k 1e-15)
(assert-equal "ji = -k" (* j i) (- 0 k) 1e-15)
(assert-equal "jk = i" (* j k) i 1e-15)
(assert-equal "kj = -i" (* k j) (- 0 i) 1e-15)
(assert-equal "ki = j" (* k i) j 1e-15)
(assert-equal "ik = -j" (* i k) (- 0 j) 1e-15)

;;; ============================================
;;; SECTION 3: BASIC ARITHMETIC
;;; ============================================

(section "BASIC ARITHMETIC")

(define q2 (quat 5 6 7 8))

(assert-close "addition" (+ q1 q2) (quat 6 8 10 12))
(assert-close "subtraction" (- q2 q1) (quat 4 4 4 4))
(assert-close "negation" (- q1) (quat -1 -2 -3 -4))

;;; Multiplication (non-commutative!)
(define prod1 (* q1 q2))
(define prod2 (* q2 q1))
(define diff-magnitude (magnitude (- prod1 prod2)))
(set! test-count (+ test-count 1))
(display test-count) (display ". multiplication is non-commutative: ")
(if (> diff-magnitude 0.1)
    (begin
      (display "✓ PASS")
      (set! pass-count (+ pass-count 1)))
    (begin
      (display "✗ FAIL (q1*q2 = q2*q1, but shouldn't!)")
      (set! fail-count (+ fail-count 1))))
(newline)

;;; ============================================
;;; SECTION 4: AUTO-SIMPLIFICATION
;;; ============================================

(section "AUTO-SIMPLIFICATION")

(assert-equal "pure real auto-simplifies" (* i i) -1 1e-15)
(assert-equal "addition simplifies" (+ (quat 1 0 0 0) (quat 2 0 0 0)) 3 1e-15)
(assert-equal "multiplication simplifies" (* (quat 2 0 0 0) (quat 3 0 0 0)) 6 1e-15)

;;; ============================================
;;; SECTION 5: TYPE PROMOTION
;;; ============================================

(section "TYPE PROMOTION")

(assert-close "real + quaternion" (+ 1 q1) (quat 2 2 3 4))
(assert-close "quaternion + real" (+ q1 1) (quat 2 2 3 4))
(assert-close "complex + quaternion" (+ (make-rectangular 1 2) (quat 3 4 5 6)) (quat 4 6 5 6))

;;; ============================================
;;; SECTION 6: CONJUGATE, MAGNITUDE, INVERSE
;;; ============================================

(section "CONJUGATE, MAGNITUDE, INVERSE")

(assert-close "conjugate" (conj q1) (quat 1 -2 -3 -4))
(assert-close "magnitude" (magnitude q1) (sqrt 30))
(assert-close "q * conj(q) = |q|²" (* q1 (conj q1)) 30)

(define q1_inv (quat-inverse q1))
(assert-close "q * q⁻¹ = 1" (* q1 q1_inv) 1)
(assert-close "q⁻¹ * q = 1" (* q1_inv q1) 1)

;;; ============================================
;;; SECTION 7: NORMALIZATION
;;; ============================================

(section "NORMALIZATION")

(define q_norm (quat-normalize q1))
(assert-close "normalized magnitude = 1" (magnitude q_norm) 1)

;;; ============================================
;;; SECTION 8: SQRT AND LOG
;;; ============================================

(section "SQRT AND LOG")

(assert-close "sqrt(4) = 2" (sqrt (quat 4 0 0 0)) 2)
(assert-close "sqrt(-1) = i" (sqrt (quat -1 0 0 0)) i)
(assert-close "sqrt(q)² = q" (* (sqrt q1) (sqrt q1)) q1)

(assert-close "log(1) = 0" (log (quat 1 0 0 0)) 0)
(assert-close "log(e) ≈ 1" (log (quat 2.71828 0 0 0)) 1)
(assert-close "exp(log(q)) = q" (exp (log q1)) q1)

;;; ============================================
;;; SECTION 9: TRIGONOMETRIC FUNCTIONS
;;; ============================================

(section "TRIGONOMETRIC FUNCTIONS")

(define q_small (quat 0.1 0.2 0.3 0.4))

(assert-close "sin(0) = 0" (sin (quat 0 0 0 0)) 0)
(assert-close "cos(0) = 1" (cos (quat 0 0 0 0)) 1)
(assert-close "tan(0) = 0" (tan (quat 0 0 0 0)) 0)

;;; Critical identity: sin²(q) + cos²(q) = 1
(define sinq (sin q_small))
(define cosq (cos q_small))
(define identity-result (+ (* sinq sinq) (* cosq cosq)))
(assert-equal "sin²(q) + cos²(q) = 1 (real part)" (quat-w identity-result) 1 1e-10)
(assert-equal "sin²(q) + cos²(q) = 1 (imag ~0)" 
              (magnitude (quat 0 (quat-x identity-result) (quat-y identity-result) (quat-z identity-result)))
              0
              1e-10)

;;; tan = sin/cos
(assert-close "tan(q) = sin(q)/cos(q)" (tan q_small) (/ sinq cosq))

;;; ============================================
;;; SECTION 10: HYPERBOLIC FUNCTIONS
;;; ============================================

(section "HYPERBOLIC FUNCTIONS")

(assert-close "sinh(0) = 0" (sinh (quat 0 0 0 0)) 0)
(assert-close "cosh(0) = 1" (cosh (quat 0 0 0 0)) 1)
(assert-close "tanh(0) = 0" (tanh (quat 0 0 0 0)) 0)

;;; Critical identity: cosh²(q) - sinh²(q) = 1
(define sinhq (sinh q_small))
(define coshq (cosh q_small))
(define hyp-identity (- (* coshq coshq) (* sinhq sinhq)))
(assert-equal "cosh²(q) - sinh²(q) = 1 (real part)" (quat-w hyp-identity) 1 1e-10)
(assert-equal "cosh²(q) - sinh²(q) = 1 (imag ~0)"
              (magnitude (quat 0 (quat-x hyp-identity) (quat-y hyp-identity) (quat-z hyp-identity)))
              0
              1e-10)

;;; tanh = sinh/cosh
(assert-close "tanh(q) = sinh(q)/cosh(q)" (tanh q_small) (/ sinhq coshq))

;;; ============================================
;;; SECTION 11: INVERSE TRIGONOMETRIC
;;; ============================================

(section "INVERSE TRIGONOMETRIC")

(define q_tiny (quat 0.05 0.1 0.15 0.2))

(assert-close "sin(asin(q)) = q" (sin (asin q_tiny)) q_tiny)
(assert-close "cos(acos(q)) = q" (cos (acos q_tiny)) q_tiny)
(assert-close "tan(atan(q)) = q" (tan (atan q_tiny)) q_tiny)

(assert-close "asin(0) = 0" (asin (quat 0 0 0 0)) 0)

;;; ============================================
;;; SECTION 12: INVERSE HYPERBOLIC
;;; ============================================

(section "INVERSE HYPERBOLIC")

(assert-close "sinh(asinh(q)) = q" (sinh (asinh q_tiny)) q_tiny)
(assert-close "cosh(acosh(1+q)) = 1+q" (cosh (acosh (+ 1 q_tiny))) (+ 1 q_tiny))
(assert-close "tanh(atanh(q)) = q" (tanh (atanh q_tiny)) q_tiny)

(assert-close "asinh(0) = 0" (asinh (quat 0 0 0 0)) 0)
(assert-close "acosh(1) = 0" (acosh (quat 1 0 0 0)) 0)
(assert-close "atanh(0) = 0" (atanh (quat 0 0 0 0)) 0)

;;; ============================================
;;; SECTION 13: AXIS-ANGLE CONVERSIONS
;;; ============================================

(section "AXIS-ANGLE CONVERSIONS")

(define axis '(0 0 1))  ; Z axis
(define angle 1.5708)   ; π/2 radians (90 degrees)

(define q_rot (quat-from-axis-angle axis angle))
(assert-close "magnitude of rotation quaternion = 1" (magnitude q_rot) 1)

(define recovered (quat-to-axis-angle q_rot))
(define recovered-axis (car recovered))
(define recovered-angle (cdr recovered))

(assert-close "recovered angle" recovered-angle angle)

;;; ============================================
;;; SECTION 14: VECTOR ROTATION
;;; ============================================

(section "VECTOR ROTATION")

(define vec '(1 0 0))  ; X axis
(define rot-90-z (quat-from-axis-angle '(0 0 1) 1.5708))
(define rotated (quat-rotate-vector rot-90-z vec))

(assert-close "rotate X to Y (x component)" (car rotated) 0)
(assert-close "rotate X to Y (y component)" (cadr rotated) 1)
(assert-close "rotate X to Y (z component)" (caddr rotated) 0)

;;; ============================================
;;; SECTION 15: SLERP INTERPOLATION
;;; ============================================

(section "SLERP INTERPOLATION")

(define q_start (quat 1 0 0 0))
(define q_end (quat-from-axis-angle '(0 1 0) 3.14159))

(assert-close "slerp at t=0" (quat-slerp q_start q_end 0) q_start)
(assert-close "slerp at t=1" (quat-slerp q_start q_end 1) q_end)

(define q_mid (quat-slerp q_start q_end 0.5))
(assert-close "slerp result is normalized" (magnitude q_mid) 1)

;;; ============================================
;;; SECTION 16: SPECIAL FUNCTIONS
;;; ============================================

(section "SPECIAL FUNCTIONS")

(assert-close "abs(q) = magnitude(q)" (abs q1) (magnitude q1))
(assert-close "proj(finite) = q" (proj q1) q1)

;;; ============================================
;;; SECTION 17: COMPLEX NUMBER COMPATIBILITY
;;; ============================================

(section "COMPLEX NUMBER COMPATIBILITY")

(define c (make-rectangular 1 2))
(define q_equiv (quat 1 2 0 0))

(assert-close "sqrt(complex) ≈ sqrt(quat)" (sqrt c) (sqrt q_equiv))
(assert-close "exp(complex) ≈ exp(quat)" (exp c) (exp q_equiv))
(assert-close "sin(complex) ≈ sin(quat)" (sin c) (sin q_equiv))
(assert-close "cos(complex) ≈ cos(quat)" (cos c) (cos q_equiv))

;;; ============================================
;;; SUMMARY
;;; ============================================

(newline)
(display "╔════════════════════════════════════════════╗") (newline)
(display "║              TEST SUMMARY                  ║") (newline)
(display "╚════════════════════════════════════════════╝") (newline)
(newline)

(display "Total tests:  ") (display test-count) (newline)
(display "Passed:       ") (display pass-count) (newline)
(display "Failed:       ") (display fail-count) (newline)
(newline)

(if (= fail-count 0)
    (begin
      (display "🎉 ALL TESTS PASSED! 🎉") (newline)
      (display "Quaternion mathematics is COMPLETE and CORRECT!") (newline))
    (begin
      (display "⚠️  SOME TESTS FAILED") (newline)
      (display "Please review the failures above.") (newline)))

(newline)
(display "Coverage:") (newline)
(display "  ✓ Construction & accessors") (newline)
(display "  ✓ Hamilton's relations") (newline)
(display "  ✓ Basic arithmetic") (newline)
(display "  ✓ Auto-simplification") (newline)
(display "  ✓ Type promotion") (newline)
(display "  ✓ Conjugate, magnitude, inverse") (newline)
(display "  ✓ Normalization") (newline)
(display "  ✓ sqrt, log, exp") (newline)
(display "  ✓ Trigonometric functions") (newline)
(display "  ✓ Hyperbolic functions") (newline)
(display "  ✓ Inverse trigonometric") (newline)
(display "  ✓ Inverse hyperbolic") (newline)
(display "  ✓ Axis-angle conversions") (newline)
(display "  ✓ Vector rotation") (newline)
(display "  ✓ SLERP interpolation") (newline)
(display "  ✓ Special functions") (newline)
(display "  ✓ Complex compatibility") (newline)
(newline)
