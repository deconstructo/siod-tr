;;; plot-utils.scm
;;; Utility functions for PLplot in SIOD-TR
;;; These work with SIOD's limitations (no named-let, etc.)


;;; Map function over list
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))

;;; Filter list by predicate
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) 
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;;; Reduce/fold left
(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f acc (car lst)) (cdr lst))))

;;; ============================================
;;; Range Generation
;;; ============================================

;;; Simple recursive range - builds list forward
(define (range start end step)
  (if (> start end)
      '()
      (cons start (range (+ start step) end step))))

;;; Alternative: range with helper (uses internal define)
(define (range-helper start end step)
  (define (helper current acc)
    (if (> current end)
        (reverse acc)
        (helper (+ current step) (cons current acc))))
  (helper start '()))

;;; Integer range: (irange 0 10) => (0 1 2 3 4 5 6 7 8 9 10)
(define (irange start end)
  (range start end 1))

;;; ============================================
;;; List Utilities
;;; ============================================

;;; Map a function returning multiple values to separate lists
(define (map-multi f lst)
  (if (null? lst)
      '(() ())
      (let* ((result (f (car lst)))
             (rest (map-multi f (cdr lst))))
        (list (cons (car result) (car rest))
              (cons (cadr result) (cadr rest))))))

;;; Zip two lists together: (zip '(1 2 3) '(4 5 6)) => ((1 4) (2 5) (3 6))
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (list (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;;; ============================================
;;; Mathematical Utilities
;;; ============================================

;;; Linear interpolation
(define (lerp a b t)
  (+ a (* t (- b a))))

;;; Clamp value between min and max
(define (clamp x min-val max-val)
  (cond ((< x min-val) min-val)
        ((> x max-val) max-val)
        (else x)))

;;; Map value from one range to another
(define (map-range value in-min in-max out-min out-max)
  (+ out-min 
     (* (- out-max out-min)
        (/ (- value in-min) (- in-max in-min)))))

;;; ============================================
;;; 2D Grid Generation
;;; ============================================

;;; Create 2D grid of values for surface plots
;;; func takes (x y) and returns z value
(define (make-grid x-min x-max nx y-min y-max ny func)
  (define dx (/ (- x-max x-min) (- nx 1)))
  (define dy (/ (- y-max y-min) (- ny 1)))
  
  (define (make-row y-index)
    (define y (+ y-min (* y-index dy)))
    (define (make-cell x-index)
      (define x (+ x-min (* x-index dx)))
      (func x y))
    (map make-cell (irange 0 (- nx 1))))
  
  (map make-row (irange 0 (- ny 1))))

;;; ============================================
;;; Common Mathematical Functions
;;; ============================================

;;; Gaussian function
(define (gaussian x mean stddev)
  (let ((z (/ (- x mean) stddev)))
    (* (/ 1.0 (* stddev (sqrt (* 2.0 3.14159))))
       (exp (* -0.5 z z)))))

;;; Sigmoid function
(define (sigmoid x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

;;; ============================================
;;; Color Utilities
;;; ============================================

;;; Convert HSV to RGB (returns list of (r g b) values 0-255)
(define (hsv->rgb h s v)
  (let* ((c (* v s))
         (h-prime (/ h 60.0))
         (x (* c (- 1.0 (abs (- (modulo h-prime 2.0) 1.0)))))
         (m (- v c))
         (rgb-prime
          (cond ((< h-prime 1) (list c x 0.0))
                ((< h-prime 2) (list x c 0.0))
                ((< h-prime 3) (list 0.0 c x))
                ((< h-prime 4) (list 0.0 x c))
                ((< h-prime 5) (list x 0.0 c))
                (else (list c 0.0 x)))))
    (map (lambda (val) (inexact->exact (truncate (* 255 (+ val m)))))
         rgb-prime)))

;;; ============================================
;;; Quick Plotting Helpers
;;; ============================================

;;; Quick line plot to PDF
(define (quick-plot filename x-data y-data title)
  (plot-device "pdf")
  (plot-output filename)
  (plot-init)
  
  (let ((x-min (apply min x-data))
        (x-max (apply max x-data))
        (y-min (apply min y-data))
        (y-max (apply max y-data)))
    (plot-env x-min x-max y-min y-max)
    (plot-labels "X" "Y" title)
    (plot-line x-data y-data))
  
  (plot-end))

;;; Quick function plot - plots f(x) over range
(define (plot-function f x-min x-max filename title)
  (let* ((x-vals (range x-min x-max (/ (- x-max x-min) 100.0)))
         (y-vals (map f x-vals)))
    (quick-plot filename x-vals y-vals title)))

;;; ============================================
;;; Example Usage
;;; ============================================

;;; Plot sine wave:
;;;   (plot-function sin 0.0 6.28 "sine.pdf" "Sine Wave")

;;; Create 2D grid:
;;;   (define grid (make-grid -1.0 1.0 10 -1.0 1.0 10 
;;;                          (lambda (x y) (+ (* x x) (* y y)))))

;;; Generate range:
;;;   (define x (range 0.0 10.0 0.5))

(display "plot-utils.scm loaded")
(newline)
