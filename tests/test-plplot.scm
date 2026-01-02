;;; test-plplot.scm
;;; Comprehensive test suite for PLplot integration in SIOD-TR
;;;
;;; Run all tests: (load "test-plplot.scm") (run-all-tests)
;;; Run specific test: (test-basic-line-plot)


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
  (display "Test Summary")
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
;;; Test 1: Version Check
;;; ============================================

(define (test-version)
  (test-start "PLplot version check")
  (let ((version (plot-version)))
    (if (> (string-length version) 0)
        (begin
          (test-pass)
          (display "  PLplot version: ")
          (display version)
          (newline))
        (test-fail "version string empty"))))

;;; ============================================
;;; Test 2: Basic Line Plot
;;; ============================================

(define (test-basic-line-plot)
  (test-start "Basic line plot")
  (plot-device "nulldriver")  ; Use null driver for headless testing
  (plot-init)
  
  (let ((x '(0.0 1.0 2.0 3.0 4.0))
        (y '(0.0 1.0 4.0 9.0 16.0)))
    (plot-env 0.0 4.0 0.0 16.0)
    (plot-labels "X" "Y" "Test Plot")
    (plot-line x y))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 3: Multiple Lines with Colors
;;; ============================================

(define (test-multiple-lines)
  (test-start "Multiple lines with colors")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(0.0 0.5 1.0 1.5 2.0))
        (y1 '(0.0 0.25 1.0 2.25 4.0))
        (y2 '(0.0 0.5 2.0 4.5 8.0)))
    
    (plot-env 0.0 2.0 0.0 8.0)
    (plot-labels "X" "Y" "Multiple Lines")
    
    (plot-color 1)
    (plot-line x y1)
    
    (plot-color 2)
    (plot-line x y2))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 4: Points and Symbols
;;; ============================================

(define (test-points)
  (test-start "Points with symbols")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(1.0 2.0 3.0 4.0 5.0))
        (y '(2.0 4.0 3.0 5.0 4.5)))
    
    (plot-env 0.0 6.0 0.0 6.0)
    (plot-labels "X" "Y" "Scatter Plot")
    
    (plot-color 3)
    (plot-points x y 17))  ; Filled circle
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 5: Histogram
;;; ============================================

(define (test-histogram)
  (test-start "Histogram")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((data '(1.2 2.3 2.5 3.1 3.4 3.8 4.2 4.5 4.8 5.1
                5.5 5.9 6.2 6.8 7.1 7.5 8.2 8.9 9.1 9.5)))
    
    (plot-env 0.0 10.0 0.0 5.0)
    (plot-labels "Value" "Count" "Histogram Test")
    
    (plot-color 4)
    (plot-histogram data 0.0 10.0 10))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 6: Error Bars
;;; ============================================

(define (test-error-bars)
  (test-start "Error bars")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(1.0 2.0 3.0 4.0 5.0))
        (y '(2.0 3.5 5.0 4.5 6.0))
        (ymin '(1.5 3.0 4.5 4.0 5.5))
        (ymax '(2.5 4.0 5.5 5.0 6.5)))
    
    (plot-env 0.0 6.0 0.0 7.0)
    (plot-labels "X" "Y" "Error Bar Test")
    
    (plot-color 15)
    (plot-error-y x ymin ymax)
    
    (plot-color 2)
    (plot-points x y 17))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 7: Log Scale
;;; ============================================

(define (test-log-scale)
  (test-start "Logarithmic scale")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(1.0 10.0 100.0 1000.0))
        (y '(1.0 10.0 100.0 1000.0)))
    
    (plot-env-log 1.0 1000.0 1.0 1000.0 30)  ; 30 = log-log
    (plot-labels "log(X)" "log(Y)" "Log-Log Plot")
    
    (plot-color 2)
    (plot-line x y))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 8: Subplots
;;; ============================================

(define (test-subplots)
  (test-start "Subplots (2x2 grid)")
  (plot-device "nulldriver")
  (plot-init)
  (plot-subplot 2 2)
  
  (let ((x '(0.0 1.0 2.0 3.0)))
    
    ; Plot 1
    (plot-env 0.0 3.0 0.0 3.0)
    (plot-labels "X" "Y" "Plot 1")
    (plot-line x '(0.0 1.0 2.0 3.0))
    
    ; Plot 2
    (plot-advance)
    (plot-env 0.0 3.0 0.0 9.0)
    (plot-labels "X" "Y^2" "Plot 2")
    (plot-line x '(0.0 1.0 4.0 9.0))
    
    ; Plot 3
    (plot-advance)
    (plot-env 0.0 3.0 0.0 27.0)
    (plot-labels "X" "Y^3" "Plot 3")
    (plot-line x '(0.0 1.0 8.0 27.0))
    
    ; Plot 4
    (plot-advance)
    (plot-env 0.0 3.0 0.0 1.0)
    (plot-labels "X" "1/Y" "Plot 4")
    (plot-line x '(1.0 0.5 0.333 0.25)))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 9: 3D Line Plot
;;; ============================================

(define (test-3d-line)
  (test-start "3D line plot")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(0.0 1.0 2.0 3.0 4.0))
        (y '(0.0 1.0 0.0 -1.0 0.0))
        (z '(0.0 1.0 4.0 9.0 16.0)))
    
    (plot-3d-init 0.0 4.0 -1.0 1.0 0.0 16.0 30.0 45.0)
    (plot-3d-box "X" "Y" "Z")
    
    (plot-color 2)
    (plot-3d-line x y z))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 10: 3D Surface
;;; ============================================

(define (test-3d-surface)
  (test-start "3D surface plot")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(0.0 1.0 2.0))
        (y '(0.0 1.0 2.0))
        (z '((0.0 1.0 2.0)
             (1.0 2.0 3.0)
             (2.0 3.0 4.0))))
    
    (plot-3d-init 0.0 2.0 0.0 2.0 0.0 4.0 30.0 45.0)
    (plot-3d-box "X" "Y" "Z")
    
    (plot-color 3)
    (plot-3d-surface x y z))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 11: PDF Output
;;; ============================================

(define (test-pdf-output)
  (test-start "PDF output")
  (plot-device "pdfqt")
  (plot-output "test-output.pdf")
  (plot-init)
  
  (let ((x '(0.0 0.5 1.0 1.5 2.0))
        (y '(0.0 0.25 1.0 2.25 4.0)))
    
    (plot-env 0.0 2.0 0.0 4.0)
    (plot-labels "X" "X^2" "PDF Test")
    (plot-color 2)
    (plot-width 2.0)
    (plot-line x y))
  
  (plot-end)
  
  ; Check if file was created
  (if (file-exists? "test-output.pdf")
      (begin
        (test-pass)
        (display "  Created: test-output.pdf")
        (newline))
      (test-fail "PDF file not created")))

;;; ============================================
;;; Test 12: Custom Colors
;;; ============================================

(define (test-custom-colors)
  (test-start "Custom RGB colors")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((x '(0.0 1.0 2.0 3.0 4.0))
        (y '(0.0 1.0 4.0 9.0 16.0)))
    
    (plot-env 0.0 4.0 0.0 16.0)
    (plot-labels "X" "Y" "Custom Color Test")
    
    (plot-color-rgb 255 0 128)  ; Hot pink
    (plot-width 3.0)
    (plot-line x y))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 13: Array Length Mismatch Error
;;; ============================================

(define (test-array-mismatch)
  (test-start "Array length mismatch error handling")
  (plot-device "nulldriver")
  (plot-init)
  
  (let ((caught #f))
    ; Try to plot arrays of different lengths
    (catch
      (lambda ()
        (plot-line '(1.0 2.0 3.0)
                   '(1.0 2.0)))
      (lambda (error-msg)
        (set! caught #t)))
    
    (plot-end)
    
    (if caught
        (test-pass)
        (test-fail "should have caught length mismatch error"))))

;;; ============================================
;;; Test 14: Sine Wave (Mathematical Function)
;;; ============================================

(define (test-sine-wave)
  (test-start "Sine wave plot")
  (plot-device "nulldriver")
  (plot-init)
  
  (define (range start end step)
    (let loop ((x start) (acc '()))
      (if (> x end)
          (reverse acc)
          (loop (+ x step) (cons x acc)))))
  
  (let ((x (range 0.0 6.28 0.1)))
    (plot-env 0.0 6.28 -1.0 1.0)
    (plot-labels "x" "sin(x)" "Sine Wave")
    
    (plot-color 4)
    (plot-line x (map sin x)))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Test 15: Background Color
;;; ============================================

(define (test-background-color)
  (test-start "Background color")
  (plot-device "nulldriver")
  (plot-background-color 240 240 240)  ; Light gray
  (plot-init)
  
  (let ((x '(0.0 1.0 2.0))
        (y '(0.0 1.0 4.0)))
    
    (plot-env 0.0 2.0 0.0 4.0)
    (plot-labels "X" "Y" "Background Test")
    (plot-line x y))
  
  (plot-end)
  (test-pass))

;;; ============================================
;;; Helper: Check if file exists
;;; ============================================

(define (file-exists? filename)
  ; Simple file existence check using system command
  (= 0 (system (string-append "test -f " filename))))

;;; ============================================
;;; Run All Tests
;;; ============================================

(define (run-all-tests)
  (display "========================================")
  (newline)
  (display "SIOD-TR PLplot Test Suite")
  (newline)
  (display "========================================")
  (newline)
  (newline)
  
  (set! test-count 0)
  (set! pass-count 0)
  (set! fail-count 0)
  
  ; Run all tests
  (test-version)
  (test-basic-line-plot)
  (test-multiple-lines)
  (test-points)
  (test-histogram)
  (test-error-bars)
  (test-log-scale)
  (test-subplots)
  (test-3d-line)
  (test-3d-surface)
  (test-pdf-output)
  (test-custom-colors)
  (test-sine-wave)
  (test-background-color)
  ; Skip error test in batch mode: (test-array-mismatch)
  
  (test-summary)
  
  ; Cleanup
  (system "rm -f test-output.pdf"))

;;; ============================================
;;; Interactive Tests (run with Qt window)
;;; ============================================

(define (demo-interactive)
  (display "Running interactive demo...")
  (newline)
  (display "Close the Qt window to continue.")
  (newline)
  
  (plot-device "qtwidget")
  (plot-init)
  
  (define (range start end step)
    (let loop ((x start) (acc '()))
      (if (> x end)
          (reverse acc)
          (loop (+ x step) (cons x acc)))))
  
  (let ((x (range 0.0 6.28 0.05)))
    (plot-env 0.0 6.28 -1.0 1.0)
    (plot-labels "x" "f(x)" "Interactive Demo: Trig Functions")
    
    (plot-color 2)
    (plot-line x (map sin x))
    
    (plot-color 3)
    (plot-line x (map cos x)))
  
  (plot-end)
  
  (display "Demo completed.")
  (newline))

;;; ============================================
;;; Usage Instructions
;;; ============================================
;;; 
;;; Load and run all tests:
;;;   (load "test-plplot.scm")
;;;   (run-all-tests)
;;;
;;; Run individual tests:
;;;   (test-basic-line-plot)
;;;   (test-sine-wave)
;;;   (test-3d-surface)
;;;
;;; Interactive demo (requires Qt):
;;;   (demo-interactive)
;;;
