;;; octonion-test.scm
;;; Test suite for octonion arithmetic in SIOD-TR
;;;
;;; Usage: (load "octonion-test.scm")

(define (test-octonion-basics)
  (puts "=== Testing Octonion Basics ===\n")
  
  ;; Create some octonions
  (define o1 (oct 1 0 0 0 0 0 0 0))  ; real unit
  (define o2 (oct 0 1 0 0 0 0 0 0))  ; i
  (define o3 (oct 0 0 1 0 0 0 0 0))  ; j
  (define o4 (oct 0 0 0 1 0 0 0 0))  ; k
  (define o5 (oct 0 0 0 0 1 0 0 0))  ; l
  
  (puts "o1 (real unit): ")
  (print o1)
  (puts "\n")
  
  (puts "o2 (i): ")
  (print o2)
  (puts "\n")
  
  (puts "o3 (j): ")
  (print o3)
  (puts "\n")
  
  ;; Test predicates
  (puts "\n(octonion? o1) => ")
  (print (octonion? o1))
  (puts "\n")
  
  (puts "(octonion? 42) => ")
  (print (octonion? 42))
  (puts "\n"))

(define (test-octonion-arithmetic)
  (puts "\n=== Testing Octonion Arithmetic ===\n")
  
  ;; Basic operations
  (define i (oct 0 1 0 0 0 0 0 0))
  (define j (oct 0 0 1 0 0 0 0 0))
  (define k (oct 0 0 0 1 0 0 0 0))
  (define l (oct 0 0 0 0 1 0 0 0))
  
  ;; Test i² = -1
  (puts "i² = ")
  (print (oct-multiply i i))
  (puts " (should be #O(-1 0 0 0 0 0 0 0))\n")
  
  ;; Test i*j = k
  (puts "i*j = ")
  (print (oct-multiply i j))
  (puts " (should be #O(0 0 0 1 0 0 0 0))\n")
  
  ;; Test j*i = -k (non-commutative!)
  (puts "j*i = ")
  (print (oct-multiply j i))
  (puts " (should be #O(0 0 0 -1 0 0 0 0))\n")
  
  ;; Test l² = -1
  (puts "l² = ")
  (print (oct-multiply l l))
  (puts " (should be #O(-1 0 0 0 0 0 0 0))\n")
  
  ;; Test i*l = il
  (puts "i*l = ")
  (print (oct-multiply i l))
  (puts " (should be #O(0 0 0 0 0 1 0 0))\n"))

(define (test-octonion-properties)
  (puts "\n=== Testing Octonion Properties ===\n")
  
  (define o (oct 1 2 3 4 5 6 7 8))
  
  ;; Test norm
  (puts "o = ")
  (print o)
  (puts "\n")
  
  (puts "|o| = ")
  (print (oct-norm o))
  (puts " (should be ~14.2829)\n")
  
  (puts "|o|² = ")
  (print (oct-norm-squared o))
  (puts " (should be 204)\n")
  
  ;; Test conjugate
  (puts "conj(o) = ")
  (print (oct-conjugate o))
  (puts "\n")
  
  ;; Test normalization
  (define o-unit (oct-normalize o))
  (puts "normalize(o) has |o| = ")
  (print (oct-norm o-unit))
  (puts " (should be ~1.0)\n"))

(define (test-non-associativity)
  (puts "\n=== Testing Non-Associativity ===\n")
  
  (define i (oct 0 1 0 0 0 0 0 0))
  (define j (oct 0 0 1 0 0 0 0 0))
  (define l (oct 0 0 0 0 1 0 0 0))
  
  ;; Compute (i*j)*l
  (define ij (oct-multiply i j))
  (define left (oct-multiply ij l))
  
  (puts "(i*j)*l = ")
  (print left)
  (puts "\n")
  
  ;; Compute i*(j*l)
  (define jl (oct-multiply j l))
  (define right (oct-multiply i jl))
  
  (puts "i*(j*l) = ")
  (print right)
  (puts "\n")
  
  (puts "These should be DIFFERENT (non-associative!)\n"))

(define (test-accessors)
  (puts "\n=== Testing Accessors ===\n")
  
  (define o (oct 1 2 3 4 5 6 7 8))
  
  (puts "o = ")
  (print o)
  (puts "\n")
  
  (puts "real part = ")
  (print (oct-real o))
  (puts "\n")
  
  (puts "e1 component = ")
  (print (oct-e1 o))
  (puts "\n")
  
  (puts "e4 component = ")
  (print (oct-e4 o))
  (puts "\n"))

(define (run-all-tests)
  (puts "\n╔════════════════════════════════════════╗\n")
  (puts "║   SIOD-TR Octonion Test Suite        ║\n")
  (puts "╚════════════════════════════════════════╝\n\n")
  
  (test-octonion-basics)
  (test-octonion-arithmetic)
  (test-octonion-properties)
  (test-non-associativity)
  (test-accessors)
  
  (puts "\n╔════════════════════════════════════════╗\n")
  (puts "║   All tests complete!                 ║\n")
  (puts "╚════════════════════════════════════════╝\n"))

;; Auto-run tests when loaded
(run-all-tests)
