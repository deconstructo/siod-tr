;;; SIOD-TR Quality of Life Utilities
;;; Essential tools for pleasant development
;;; Load at startup: (load "qol.scm")

;;; ============================================
;;; Compatibility Fixes
;;; ============================================

;;; Define defined? for SIOD (it doesn't have it built-in)
(define (defined? sym)
  "Check if a symbol is defined"
  (symbol-bound? sym (the-environment)))

(define (newline) (display "\n"))

;;; Add reverse if missing
(if (not (defined? 'reverse))
    (define (reverse lst)
      (define (rev-helper l acc)
        (if (null? l)
            acc
            (rev-helper (cdr l) (cons (car l) acc))))
      (rev-helper lst '())))

;;; Modulo is  defined as % in siod-tr
(define modulo %)
(define fmod %)


;;; ============================================
;;; List Utilities
;;; ============================================

(define (range start end)
  "Generate list of numbers from start to end"
  (define (range-helper n)
    (if (> n end)
        '()
        (cons n (range-helper (+ n 1)))))
  (range-helper start))

(define (iota n)
  "Generate list (0 1 2 ... n-1)"
  (range 0 (- n 1)))

(define (take n lst)
  "Take first n elements from list"
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  "Drop first n elements from list"
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

(define (filter pred lst)
  "Keep only elements where pred returns true"
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (fold proc init lst)
  "Fold list with proc: (fold + 0 '(1 2 3)) => 6"
  (if (null? lst)
      init
      (fold proc (proc init (car lst)) (cdr lst))))

(define (sum lst)
  "Sum all numbers in list"
  (fold + 0 lst))

(define (product lst)
  "Product of all numbers in list"
  (fold * 1 lst))

;;; ============================================
;;; String Utilities
;;; ============================================

(define (string-split str delim)
  "Split string by delimiter (simple version)"
  ;; Note: This is a placeholder - actual implementation
  ;; depends on SIOD's string functions
  (list str))  ; Simplified for now

(define (string-join strings sep)
  "Join list of strings with separator"
  ;; Placeholder - depends on string functions
  (if (null? strings)
      ""
      (car strings)))

;;; ============================================
;;; Convenience Functions
;;; ============================================

(define (compose f g)
  "Function composition: (compose f g) => λx.f(g(x))"
  (lambda (x) (f (g x))))

(define (identity x)
  "Identity function"
  x)

(define (constantly x)
  "Return function that always returns x"
  (lambda args x))

(define (partial f . args)
  "Partial application"
  (lambda rest
    (apply f (append args rest))))

;;; ============================================
;;; Debugging & Inspection
;;; ============================================

(define (inspect obj)
  "Quick inspection of object"
  (cond ((null? obj) (display "()"))
        ((pair? obj) 
         (display "(")
         (display (car obj))
         (display " ...)")
         (newline))
        ((number? obj)
         (display "NUMBER: ")
         (display obj)
         (newline))
        ((symbol? obj)
         (display "SYMBOL: ")
         (display obj)
         (newline))
        (else
         (display obj)
         (newline))))

(define (type-of obj)
  "Return type of object as symbol"
  (cond ((null? obj) 'null)
        ((pair? obj) 'pair)
        ((number? obj) 'number)
        ((symbol? obj) 'symbol)
        ((string? obj) 'string)
        (else 'unknown)))

;;; ============================================
;;; Timing & Performance
;;; ============================================

;;; Function version - works without macro support
(define (time-fn thunk)
  "Time execution of a thunk (zero-argument function)"
  (let ((start (get-time)))
    (let ((result (thunk)))
      (let ((elapsed (- (get-time) start)))
        (display "Elapsed: ")
        (display elapsed)
        (display " seconds")
        (newline)
        result))))

;;; Macro version - only if SIOD supports define-macro
;;; Uncomment this if (define-macro (test x) `(+ ,x 1)) works:
;;
;; (define-macro (time expr)
;;   "Time execution of expression"
;;   `(let ((start (get-time)))
;;      (let ((result ,expr))
;;        (let ((elapsed (- (get-time) start)))
;;          (display "Elapsed: ")
;;          (display elapsed)
;;          (display " seconds")
;;          (newline)
;;          result))))

;;; ============================================
;;; List Processing
;;; ============================================

(define (zip lst1 lst2)
  "Zip two lists into pairs: (zip '(1 2) '(a b)) => ((1 a) (2 b))"
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (enumerate lst)
  "Add indices: (enumerate '(a b c)) => ((0 a) (1 b) (2 c))"
  (zip (iota (length lst)) lst))

(define (flatten lst)
  "Flatten nested lists"
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else
         (cons (car lst) (flatten (cdr lst))))))

;;; ============================================
;;; Higher-Order Utilities
;;; ============================================

(define (curry f)
  "Curry a two-argument function"
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (flip f)
  "Flip arguments of two-argument function"
  (lambda (x y)
    (f y x)))

;;; ============================================
;;; Math Utilities
;;; ============================================

(define (clamp x min-val max-val)
  "Clamp x between min and max"
  (cond ((< x min-val) min-val)
        ((> x max-val) max-val)
        (else x)))

(define (lerp a b t)
  "Linear interpolation: lerp(a, b, 0.5) => midpoint"
  (+ a (* t (- b a))))

(define (square x)
  "Square of x"
  (* x x))

(define (cube x)
  "Cube of x"
  (* x x x))

;;; ============================================
;;; Predicates
;;; ============================================

(define (between? x min-val max-val)
  "Check if x is between min and max (inclusive)"
  (and (>= x min-val) (<= x max-val)))

(define (all? pred lst)
  "Check if predicate is true for all elements"
  (cond ((null? lst) #t)
        ((pred (car lst)) (all? pred (cdr lst)))
        (else #f)))

(define (any? pred lst)
  "Check if predicate is true for any element"
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any? pred (cdr lst)))))

(define (none? pred lst)
  "Check if predicate is false for all elements"
  (not (any? pred lst)))

;;; ============================================
;;; Startup Message
;;; ============================================

(display "SIOD-TR QoL Utilities loaded")
(newline)
(display "Try: (range 1 10), (filter even? '(1 2 3 4 5 6))")
(newline)
(display "     (time-fn (lambda () (sum (range 1 100))))")
(newline)
