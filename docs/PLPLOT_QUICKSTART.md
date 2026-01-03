# PLplot Quickstart Guide for SIOD-TR

## 5-Minute Quick Start

### Installation

```bash
# Ubuntu 22.04
sudo apt install libplplot-dev plplot-driver-qt

# macOS
brew install plplot
```

### Build SIOD-TR with PLplot

Add to your Makefile:
```makefile
PLPLOT_CFLAGS := $(shell pkg-config --cflags plplot)
PLPLOT_LIBS := $(shell pkg-config --libs plplot)
CFLAGS += $(PLPLOT_CFLAGS)
LIBS += $(PLPLOT_LIBS)
OBJS += siod_plplot.o
```

Then rebuild:
```bash
make clean
make
```

### Your First Plot (Copy-Paste Ready)

```scheme
; Simple sine wave
(plot-device "qtwidget")
(plot-init)

(let ((x '(0.0 0.5 1.0 1.5 2.0 2.5 3.0))
      (y '(0.0 0.48 0.84 1.0 0.91 0.60 0.14)))
  (plot-env 0.0 3.0 0.0 1.0)
  (plot-labels "x" "sin(x)" "Hello PLplot!")
  (plot-line x y))

(plot-end)
```

**That's it!** A Qt window will appear with your plot.

---

## Common Plotting Tasks

### Save Plot to PDF

```scheme
(plot-device "pdfqt")
(plot-output "my-plot.pdf")
(plot-init)

; ... your plotting code ...

(plot-end)
; PDF saved!
```

### Multiple Lines

```scheme
(plot-device "qtwidget")
(plot-init)

(let ((x '(0 1 2 3 4)))
  (plot-env 0 4 0 16)
  (plot-labels "x" "y" "Multiple Lines")
  
  (plot-color 1)  ; Red
  (plot-line x '(0 1 4 9 16))
  
  (plot-color 9)  ; Blue
  (plot-line x '(0 2 8 18 32)))

(plot-end)
```

### Scatter Plot

```scheme
(plot-device "qtwidget")
(plot-init)

(let ((x '(1 2 3 4 5))
      (y '(2.1 3.9 6.2 7.8 10.1)))
  (plot-env 0 6 0 12)
  (plot-labels "X" "Y" "Scatter Plot")
  
  (plot-color 2)
  (plot-points x y 17))  ; 17 = filled circle

(plot-end)
```

### Histogram

```scheme
(plot-device "qtwidget")
(plot-init)

(let ((data '(1.2 2.3 2.7 3.1 3.5 4.2 4.8 5.1 5.9 6.2 7.1 7.8 8.3 9.1)))
  (plot-env 0 10 0 5)
  (plot-labels "Value" "Count" "Histogram")
  
  (plot-color 3)
  (plot-histogram data 0 10 10))  ; 10 bins from 0 to 10

(plot-end)
```

### Error Bars

```scheme
(plot-device "qtwidget")
(plot-init)

(let ((x '(1 2 3 4 5))
      (y '(2.0 3.5 5.0 4.5 6.0))
      (y-low '(1.5 3.0 4.5 4.0 5.5))
      (y-high '(2.5 4.0 5.5 5.0 6.5)))
  
  (plot-env 0 6 0 7)
  (plot-labels "X" "Y" "Error Bars")
  
  (plot-color 15)  ; Gray
  (plot-error-y x y-low y-high)
  
  (plot-color 1)   ; Red
  (plot-points x y 17))

(plot-end)
```

### 4 Subplots

```scheme
(plot-device "qtwidget")
(plot-init)
(plot-subplot 2 2)  ; 2x2 grid

; Plot 1
(plot-env 0 3 0 3)
(plot-labels "x" "x" "Linear")
(plot-line '(0 1 2 3) '(0 1 2 3))

; Plot 2
(plot-advance)
(plot-env 0 3 0 9)
(plot-labels "x" "x²" "Square")
(plot-line '(0 1 2 3) '(0 1 4 9))

; Plot 3
(plot-advance)
(plot-env 0 3 0 27)
(plot-labels "x" "x³" "Cube")
(plot-line '(0 1 2 3) '(0 1 8 27))

; Plot 4
(plot-advance)
(plot-env 0 3 0 20)
(plot-labels "x" "eˣ" "Exponential")
(plot-line '(0 1 2 3) '(1.0 2.72 7.39 20.09))

(plot-end)
```

---

## Utility Functions

### Generate Range of Values

```scheme
(define (range start end step)
  (let loop ((x start) (acc '()))
    (if (> x end)
        (reverse acc)
        (loop (+ x step) (cons x acc)))))

; Usage:
(range 0.0 2.0 0.5)  ; => (0.0 0.5 1.0 1.5 2.0)
```

### Map Function Over List

```scheme
(define (map-list f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map-list f (cdr lst)))))

; Usage:
(map-list sin '(0.0 1.57 3.14))  ; => sine of each value
```

---

## Real-World Examples

### Example 1: Plot Mathematical Function

```scheme
; Plot sin(x) from 0 to 2π
(define (plot-sine)
  (define (range start end step)
    (let loop ((x start) (acc '()))
      (if (> x end)
          (reverse acc)
          (loop (+ x step) (cons x acc)))))
  
  (plot-device "qtwidget")
  (plot-init)
  
  (let ((x (range 0.0 6.28 0.1)))
    (plot-env 0.0 6.28 -1.0 1.0)
    (plot-labels "x (radians)" "sin(x)" "Sine Wave")
    
    (plot-color 2)
    (plot-width 2.0)
    (plot-line x (map sin x)))
  
  (plot-end))

; Run it:
(plot-sine)
```

### Example 2: Experimental Data Analysis

```scheme
; Plot experimental data with trend line
(define (plot-experiment)
  (plot-device "pdfqt")
  (plot-output "experiment.pdf")
  (plot-init)
  
  ; Measured data
  (let ((time '(0.0 1.0 2.0 3.0 4.0 5.0))
        (temp '(20.0 25.3 30.1 35.4 40.2 45.0))
        (temp-err-low '(19.5 24.8 29.6 34.9 39.7 44.5))
        (temp-err-high '(20.5 25.8 30.6 35.9 40.7 45.5)))
    
    (plot-env 0.0 5.0 15.0 50.0)
    (plot-labels "Time (min)" "Temperature (°C)" "Heating Experiment")
    
    ; Error bars
    (plot-color 7)
    (plot-error-y time temp-err-low temp-err-high)
    
    ; Data points
    (plot-color 1)
    (plot-points time temp 17)
    
    ; Trend line (linear fit: T = 20 + 5t)
    (plot-color 9)
    (plot-width 1.0)
    (let ((t '(0.0 5.0))
          (trend '(20.0 45.0)))
      (plot-line t trend)))
  
  (plot-end)
  (display "Saved to experiment.pdf")
  (newline))

; Run it:
(plot-experiment)
```

### Example 3: Compare Algorithms

```scheme
; Compare sorting algorithm performance
(define (plot-algorithm-comparison)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Size of input data
  (let ((sizes '(10 50 100 500 1000 5000))
        ; Time in milliseconds
        (quicksort '(0.1 0.8 1.9 12.5 28.3 180.2))
        (bubblesort '(0.2 4.5 18.2 450.1 1800.5 45000.0)))
    
    (plot-env 0 5000 0 50000)
    (plot-labels "Input Size" "Time (ms)" "Algorithm Comparison")
    
    ; Quicksort
    (plot-color 2)
    (plot-width 2.0)
    (plot-line sizes quicksort)
    (plot-text 4000 5000 "QuickSort")
    
    ; Bubble sort
    (plot-color 1)
    (plot-width 2.0)
    (plot-line sizes bubblesort)
    (plot-text 3000 35000 "BubbleSort"))
  
  (plot-end))

; Run it:
(plot-algorithm-comparison)
```

### Example 4: 3D Surface Plot

```scheme
; Plot a simple 3D surface: z = x² + y²
(define (plot-paraboloid)
  (plot-device "qtwidget")
  (plot-init)
  
  (let ((x '(-1.0 0.0 1.0))
        (y '(-1.0 0.0 1.0))
        ; z grid: each row is z values for fixed y, varying x
        (z '((2.0 1.0 2.0)    ; y=-1: z = x²+1
             (1.0 0.0 1.0)    ; y=0:  z = x²
             (2.0 1.0 2.0)))) ; y=1:  z = x²+1
    
    (plot-3d-init -1.0 1.0 -1.0 1.0 0.0 2.0 30.0 45.0)
    (plot-3d-box "X" "Y" "Z")
    
    (plot-color 3)
    (plot-3d-surface x y z))
  
  (plot-end))

; Run it:
(plot-paraboloid)
```

### Example 5: Mandelbrot Iteration Analysis

```scheme
; Analyze Mandelbrot set iterations
(define (mandelbrot-iter cx cy max-iter)
  (let loop ((zx 0.0) (zy 0.0) (iter 0))
    (let ((zx2 (* zx zx))
          (zy2 (* zy zy)))
      (if (or (>= iter max-iter)
              (> (+ zx2 zy2) 4.0))
          iter
          (loop (+ (- zx2 zy2) cx)
                (+ (* 2.0 zx zy) cy)
                (+ iter 1))))))

(define (plot-mandelbrot-scan)
  (plot-device "pdfqt")
  (plot-output "mandelbrot-scan.pdf")
  (plot-init)
  
  (define (range start end step)
    (let loop ((x start) (acc '()))
      (if (> x end)
          (reverse acc)
          (loop (+ x step) (cons x acc)))))
  
  ; Scan along real axis at imaginary = 0
  (let* ((x-vals (range -2.0 1.0 0.02))
         (iters (map (lambda (x) (mandelbrot-iter x 0.0 100)) x-vals)))
    
    (plot-env -2.0 1.0 0.0 100.0)
    (plot-labels "Real Axis" "Iterations" "Mandelbrot: Scan at Im=0")
    
    (plot-color 4)
    (plot-width 1.5)
    (plot-line x-vals iters))
  
  (plot-end)
  (display "Saved to mandelbrot-scan.pdf")
  (newline))

; Run it:
(plot-mandelbrot-scan)
```

---

## Color Reference

Quick color index reference:

```scheme
(plot-color 0)   ; Black
(plot-color 1)   ; Red
(plot-color 2)   ; Yellow
(plot-color 3)   ; Green
(plot-color 4)   ; Aquamarine
(plot-color 9)   ; Blue
(plot-color 11)  ; Cyan
(plot-color 13)  ; Magenta
(plot-color 15)  ; White

; Custom color
(plot-color-rgb 255 128 0)  ; Orange
```

---

## Device Types

```scheme
; Interactive display
(plot-device "qtwidget")  ; Best quality, Qt window
(plot-device "xwin")      ; X11 window (Linux)

; File output
(plot-device "pdfqt")     ; PDF via Qt (recommended)
(plot-device "svgqt")     ; SVG vector graphics
(plot-device "pngqt")     ; PNG raster image

; Testing
(plot-device "nulldriver") ; No output (for tests)
```

---

## Symbol Codes for plot-points

```scheme
(plot-points x y 1)   ; Dot
(plot-points x y 2)   ; Plus (+)
(plot-points x y 3)   ; Asterisk (*)
(plot-points x y 4)   ; Circle
(plot-points x y 5)   ; X
(plot-points x y 17)  ; Filled circle (recommended)
(plot-points x y 18)  ; Filled square
```

---

## Common Mistakes

### ❌ Wrong: Forgetting plot-init

```scheme
(plot-device "qtwidget")
; Missing: (plot-init)
(plot-line x y)  ; ERROR!
(plot-end)
```

### ✅ Right:

```scheme
(plot-device "qtwidget")
(plot-init)  ; Don't forget!
(plot-line x y)
(plot-end)
```

---

### ❌ Wrong: Different array lengths

```scheme
(plot-line '(1 2 3) '(4 5))  ; ERROR: lengths don't match
```

### ✅ Right:

```scheme
(plot-line '(1 2 3) '(4 5 6))  ; Same length: OK
```

---

### ❌ Wrong: Axis range doesn't include data

```scheme
; Data ranges from 0-100
(plot-env 0 10 0 10)  ; Too small!
(plot-line x y)       ; Won't see data
```

### ✅ Right:

```scheme
; Data ranges from 0-100
(plot-env 0 100 0 100)  ; Includes all data
(plot-line x y)
```

---

## Next Steps

1. **Run the test suite:**
   ```scheme
   (load "test-plplot.scm")
   (run-all-tests)
   ```

2. **Read full documentation:**
   See `PLPLOT_DOCUMENTATION.md` for complete function reference

3. **Explore examples:**
   Check `plplot-examples.scm` for more complex examples

4. **Integrate with other SIOD-TR features:**
   - Combine with Raylib for interactive controls
   - Use with quaternion library for 3D visualization
   - Plot results from numerical algorithms

---

## Getting Help

**Check version:**
```scheme
(plot-version)
```

**Verify installation:**
```bash
pkg-config --modversion plplot
```

**Run tests:**
```scheme
(load "test-plplot.scm")
(run-all-tests)
```

**Try minimal example:**
```scheme
(plot-device "qtwidget")
(plot-init)
(plot-env 0 1 0 1)
(plot-line '(0 1) '(0 1))
(plot-end)
```

If this works, your installation is correct!

---

**Happy Plotting!**

The PLplot integration brings professional scientific plotting to SIOD-TR. Start with these examples and build from there. Most plots need just 5-10 lines of code.
