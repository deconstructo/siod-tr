# PLplot Library Documentation for SIOD-TR

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Function Reference](#function-reference)
5. [Examples](#examples)
6. [Advanced Topics](#advanced-topics)
7. [Troubleshooting](#troubleshooting)

---

## Overview

The PLplot library integration provides SIOD-TR with professional scientific plotting capabilities, including:

- **2D plots**: Line plots, scatter plots, histograms, error bars
- **3D plots**: Surface plots, mesh plots, 3D lines
- **Multiple output formats**: Interactive Qt windows, PDF, SVG, PNG
- **Publication quality**: Antialiased graphics, Unicode support, customizable colors
- **Multi-plot layouts**: Subplots, multiple pages

### Design Philosophy

- **Lists as arrays**: Data is passed as SIOD lists, converted to C arrays at the boundary
- **Simple API**: Most plots need just 4-5 function calls
- **Flexible output**: Switch between interactive display and file output easily

---

## Installation

### Ubuntu 22.04

```bash
sudo apt install libplplot-dev plplot-driver-qt
```

### macOS (Homebrew)

```bash
brew install plplot
```

### Verification

After installing, verify PLplot is available:

```bash
pkg-config --modversion plplot
pkg-config --libs plplot
```

From within SIOD:
```scheme
(plot-version)  ; Should return version string like "5.15.0"
```

---

## Quick Start

### Your First Plot

```scheme
; Load PLplot library (after SIOD-TR is built with PLplot support)
(require-so (so-ext "plplot))


(plot-init)                       ; Initialize

(let ((x '(0.0 1.0 2.0 3.0 4.0))
      (y '(0.0 1.0 4.0 9.0 16.0)))
  
  (plot-env 0.0 4.0 0.0 16.0)     ; Set axis ranges
  (plot-labels "X" "X²" "My First Plot")
  (plot-line x y))                ; Draw the line

(plot-end)                        ; Close (shows window)
```

### Save to PDF

```scheme
(plot-device "pdfqt")             ; PDF output
(plot-output "my-plot.pdf")       ; Filename
(plot-init)

(let ((x '(0.0 1.0 2.0 3.0 4.0))
      (y '(0.0 1.0 4.0 9.0 16.0)))
  
c  (plot-env 0.0 4.0 0.0 16.0)
  (plot-labels "X" "X²" "Saved to PDF")
  (plot-line x y))

(plot-end)
```

---

## Function Reference

### Setup and Initialization

#### `(plot-init)`
Initialize PLplot. Call after setting device and output filename.

**Returns:** `NIL`

**Example:**
```scheme
(plot-init)
```

---

#### `(plot-end)`
Close PLplot and finalize output. For Qt windows, this displays the window.

**Returns:** `NIL`

**Example:**
```scheme
(plot-end)
```

---

#### `(plot-device name)`
Set the output device.

**Parameters:**
- `name` (string): Device name
  - `"qtwidget"` - Interactive Qt window
  - `"pdfqt"` - PDF file via Qt
  - `"svgqt"` - SVG file via Qt
  - `"pngqt"` - PNG file via Qt
  - `"xwin"` - X11 window (Linux)
  - `"nulldriver"` - No output (for testing)

**Returns:** `NIL`

**Example:**
```scheme
(plot-device "qtwidget")
```

---

#### `(plot-output filename)`
Set output filename for file-based devices.

**Parameters:**
- `filename` (string): Output filename

**Returns:** `NIL`

**Example:**
```scheme
(plot-output "graph.pdf")
```

---

#### `(plot-version)`
Get PLplot version string.

**Returns:** String with version

**Example:**
```scheme
(plot-version)  ; => "5.15.0"
```

---

### Plot Environment

#### `(plot-env xmin xmax ymin ymax)`
Set up 2D plot environment with axis ranges. Draws box, axes, and tick marks.

**Parameters:**
- `xmin`, `xmax` (number): X-axis range
- `ymin`, `ymax` (number): Y-axis range

**Returns:** `NIL`

**Example:**
```scheme
(plot-env 0.0 10.0 -1.0 1.0)
```

---

#### `(plot-env-log xmin xmax ymin ymax axis-type)`
Set up plot environment with logarithmic axes.

**Parameters:**
- `xmin`, `xmax`, `ymin`, `ymax` (number): Axis ranges
- `axis-type` (integer):
  - `0` - Linear both axes
  - `10` - Logarithmic X, linear Y
  - `20` - Linear X, logarithmic Y
  - `30` - Logarithmic both axes

**Returns:** `NIL`

**Example:**
```scheme
(plot-env-log 1.0 1000.0 1.0 1000.0 30)  ; Log-log plot
```

---

#### `(plot-labels xlabel ylabel title)`
Add axis labels and plot title.

**Parameters:**
- `xlabel` (string): X-axis label
- `ylabel` (string): Y-axis label
- `title` (string): Plot title

**Returns:** `NIL`

**Example:**
```scheme
(plot-labels "Time (s)" "Velocity (m/s)" "Rocket Launch")
```

---

### Color and Style

#### `(plot-color index)`
Set drawing color from default color map (0-15).

**Parameters:**
- `index` (integer): Color index
  - 0 = black, 1 = red, 2 = yellow, 3 = green
  - 4 = aquamarine, 5 = pink, 6 = wheat, 7 = grey
  - 8 = brown, 9 = blue, 10 = BlueViolet, 11 = cyan
  - 12 = turquoise, 13 = magenta, 14 = salmon, 15 = white

**Returns:** `NIL`

**Example:**
```scheme
(plot-color 2)  ; Yellow
```

---

#### `(plot-color-rgb r g b)`
Set custom RGB color (sets and uses color index 15).

**Parameters:**
- `r`, `g`, `b` (integer): RGB values (0-255)

**Returns:** `NIL`

**Example:**
```scheme
(plot-color-rgb 255 128 0)  ; Orange
```

---

#### `(plot-width width)`
Set line width.

**Parameters:**
- `width` (number): Line width (default is 1.0)

**Returns:** `NIL`

**Example:**
```scheme
(plot-width 2.5)  ; Thick line
```

---

#### `(plot-background-color r g b)`
Set background color. Must be called before `plot-init`.

**Parameters:**
- `r`, `g`, `b` (integer): RGB values (0-255)

**Returns:** `NIL`

**Example:**
```scheme
(plot-background-color 240 240 255)  ; Light blue
```

---

### 2D Plotting

#### `(plot-line x-list y-list)`
Draw a line plot connecting points.

**Parameters:**
- `x-list` (list): X coordinates
- `y-list` (list): Y coordinates (must be same length as x-list)

**Returns:** `NIL`

**Example:**
```scheme
(plot-line '(0 1 2 3) '(0 1 4 9))
```

---

#### `(plot-points x-list y-list symbol)`
Draw points with specified symbol.

**Parameters:**
- `x-list` (list): X coordinates
- `y-list` (list): Y coordinates
- `symbol` (integer): Symbol code
  - 1 = dot, 2 = +, 3 = *, 4 = circle, 5 = X
  - 17 = filled circle, 18 = filled square

**Returns:** `NIL`

**Example:**
```scheme
(plot-points '(1 2 3 4) '(2 4 3 5) 17)  ; Filled circles
```

---

#### `(plot-histogram data-list datmin datmax nbins)`
Draw histogram of data.

**Parameters:**
- `data-list` (list): Data values
- `datmin`, `datmax` (number): Data range
- `nbins` (integer): Number of bins

**Returns:** `NIL`

**Example:**
```scheme
(plot-histogram '(1.2 2.5 3.1 3.8 4.2 5.5 6.1) 0.0 10.0 10)
```

---

#### `(plot-error-y x-list ymin-list ymax-list)`
Draw vertical error bars.

**Parameters:**
- `x-list` (list): X coordinates
- `ymin-list` (list): Lower bounds
- `ymax-list` (list): Upper bounds

**Returns:** `NIL`

**Example:**
```scheme
(plot-error-y '(1 2 3) '(1.5 2.8 3.2) '(2.5 3.2 3.8))
```

---

#### `(plot-error-x xmin-list xmax-list y-list)`
Draw horizontal error bars.

**Parameters:**
- `xmin-list` (list): Lower bounds
- `xmax-list` (list): Upper bounds
- `y-list` (list): Y coordinates

**Returns:** `NIL`

**Example:**
```scheme
(plot-error-x '(0.8 1.9 2.8) '(1.2 2.1 3.2) '(2 3 4))
```

---

### 3D Plotting

#### `(plot-3d-init xmin xmax ymin ymax zmin zmax altitude azimuth)`
Set up 3D plot environment.

**Parameters:**
- `xmin`, `xmax`, `ymin`, `ymax`, `zmin`, `zmax` (number): 3D axis ranges
- `altitude` (number): Viewing altitude in degrees (0-90)
- `azimuth` (number): Viewing azimuth in degrees (0-360)

**Returns:** `NIL`

**Example:**
```scheme
(plot-3d-init -1.0 1.0 -1.0 1.0 0.0 1.0 30.0 45.0)
```

---

#### `(plot-3d-box xlabel ylabel zlabel)`
Draw 3D box with axis labels.

**Parameters:**
- `xlabel`, `ylabel`, `zlabel` (string): Axis labels

**Returns:** `NIL`

**Example:**
```scheme
(plot-3d-box "X" "Y" "Z")
```

---

#### `(plot-3d-line x-list y-list z-list)`
Draw 3D line connecting points.

**Parameters:**
- `x-list`, `y-list`, `z-list` (list): 3D coordinates

**Returns:** `NIL`

**Example:**
```scheme
(plot-3d-line '(0 1 2) '(0 1 0) '(0 1 4))
```

---

#### `(plot-3d-surface x-list y-list z-grid)`
Draw 3D surface plot.

**Parameters:**
- `x-list` (list): X coordinates (length nx)
- `y-list` (list): Y coordinates (length ny)
- `z-grid` (list of lists): 2D grid of Z values (ny rows × nx columns)

**Returns:** `NIL`

**Example:**
```scheme
(plot-3d-surface 
  '(0.0 1.0 2.0)
  '(0.0 1.0 2.0)
  '((0.0 1.0 2.0)
    (1.0 2.0 3.0)
    (2.0 3.0 4.0)))
```

---

#### `(plot-3d-mesh x-list y-list z-grid)`
Draw 3D wireframe mesh.

**Parameters:** Same as `plot-3d-surface`

**Returns:** `NIL`

**Example:**
```scheme
(plot-3d-mesh 
  '(0.0 1.0 2.0)
  '(0.0 1.0 2.0)
  '((0.0 1.0 4.0)
    (1.0 2.0 5.0)
    (4.0 5.0 8.0)))
```

---

### Multi-Plot Layout

#### `(plot-subplot nx ny)`
Divide page into nx×ny grid of subplots. Call before `plot-init`.

**Parameters:**
- `nx`, `ny` (integer): Grid dimensions

**Returns:** `NIL`

**Example:**
```scheme
(plot-subplot 2 2)  ; 2×2 grid (4 plots)
```

---

#### `(plot-advance)`
Advance to next subplot in grid.

**Returns:** `NIL`

**Example:**
```scheme
(plot-advance)  ; Move to next plot position
```

---

### Text and Annotations

#### `(plot-text x y text)`
Draw text at world coordinates.

**Parameters:**
- `x`, `y` (number): Position in world coordinates
- `text` (string): Text to draw

**Returns:** `NIL`

**Example:**
```scheme
(plot-text 2.5 5.0 "Peak")
```

---

### Utility Functions

#### `(plot-clear)`
Clear current plot.

**Returns:** `NIL`

---

#### `(plot-flush)`
Flush plot output buffer.

**Returns:** `NIL`

---

## Examples

### Example 1: Sine and Cosine

```scheme
(define (range start end step)
  (let loop ((x start) (acc '()))
    (if (> x end)
        (reverse acc)
        (loop (+ x step) (cons x acc)))))

(define (plot-trig)
  (plot-device "qtwidget")
  (plot-init)
  
  (let ((x (range 0.0 6.28 0.05)))
    (plot-env 0.0 6.28 -1.0 1.0)
    (plot-labels "x" "f(x)" "Trigonometric Functions")
    
    ; Sine in red
    (plot-color 1)
    (plot-line x (map sin x))
    
    ; Cosine in blue
    (plot-color 9)
    (plot-line x (map cos x)))
  
  (plot-end))
```

### Example 2: Experimental Data with Error Bars

```scheme
(define (plot-experiment)
  (plot-device "pdfqt")
  (plot-output "experiment.pdf")
  (plot-init)
  
  (let ((time '(0.0 1.0 2.0 3.0 4.0))
        (distance '(0.0 4.9 19.6 44.1 78.4))
        (err-low '(0.0 4.5 18.9 42.8 76.2))
        (err-high '(0.0 5.3 20.3 45.4 80.6)))
    
    (plot-env 0.0 4.0 0.0 90.0)
    (plot-labels "Time (s)" "Distance (m)" "Free Fall Experiment")
    
    ; Error bars in gray
    (plot-color 7)
    (plot-error-y time err-low err-high)
    
    ; Data points
    (plot-color 1)
    (plot-points time distance 17))
  
  (plot-end))
```

### Example 3: Histogram of Random Data

```scheme
(define (plot-distribution)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Generate some sample data (simplified normal distribution)
  (let ((data '(2.3 3.1 3.5 3.8 4.0 4.2 4.5 4.7 5.0 5.2
                5.5 5.8 6.0 6.3 6.5 6.8 7.0 7.5 7.8 8.1)))
    
    (plot-env 0.0 10.0 0.0 6.0)
    (plot-labels "Value" "Frequency" "Distribution")
    
    (plot-color 3)
    (plot-histogram data 0.0 10.0 20))
  
  (plot-end))
```

### Example 4: 3D Surface

```scheme
(define (plot-surface)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Simple 3×3 surface
  (let ((x '(-1.0 0.0 1.0))
        (y '(-1.0 0.0 1.0))
        (z '((2.0  1.0  2.0)
             (1.0  0.0  1.0)
             (2.0  1.0  2.0))))
    
    (plot-3d-init -1.0 1.0 -1.0 1.0 0.0 2.0 30.0 45.0)
    (plot-3d-box "X" "Y" "Z")
    
    (plot-color 2)
    (plot-3d-surface x y z))
  
  (plot-end))
```

### Example 5: Multi-Panel Plot

```scheme
(define (plot-panels)
  (plot-device "pdfqt")
  (plot-output "panels.pdf")
  (plot-init)
  (plot-subplot 2 2)
  
  (define (range start end step)
    (let loop ((x start) (acc '()))
      (if (> x end)
          (reverse acc)
          (loop (+ x step) (cons x acc)))))
  
  (let ((x (range 0.0 3.0 0.1)))
    
    ; Panel 1: Linear
    (plot-env 0.0 3.0 0.0 3.0)
    (plot-labels "x" "x" "Linear")
    (plot-line x x)
    
    ; Panel 2: Square
    (plot-advance)
    (plot-env 0.0 3.0 0.0 9.0)
    (plot-labels "x" "x²" "Quadratic")
    (plot-line x (map (lambda (v) (* v v)) x))
    
    ; Panel 3: Cube
    (plot-advance)
    (plot-env 0.0 3.0 0.0 27.0)
    (plot-labels "x" "x³" "Cubic")
    (plot-line x (map (lambda (v) (* v v v)) x))
    
    ; Panel 4: Exponential
    (plot-advance)
    (plot-env 0.0 3.0 0.0 20.0)
    (plot-labels "x" "e^x" "Exponential")
    (plot-line x (map exp x)))
  
  (plot-end))
```

---

## Advanced Topics

### Generating Data

Helper function to create ranges:

```scheme
(define (range start end step)
  (let loop ((x start) (acc '()))
    (if (> x end)
        (reverse acc)
        (loop (+ x step) (cons x acc)))))

(define x (range 0.0 10.0 0.1))  ; 0.0, 0.1, 0.2, ... 10.0
```

### Generating 2D Grids for Surfaces

```scheme
(define (make-grid nx ny func)
  (let ((dx (/ 2.0 (- nx 1)))
        (dy (/ 2.0 (- ny 1))))
    (let loop-y ((iy 0) (grid '()))
      (if (>= iy ny)
          (reverse grid)
          (let ((y (+ -1.0 (* iy dy))))
            (let loop-x ((ix 0) (row '()))
              (if (>= ix nx)
                  (loop-y (+ iy 1) (cons (reverse row) grid))
                  (let ((x (+ -1.0 (* ix dx))))
                    (loop-x (+ ix 1) (cons (func x y) row))))))))))

; Example usage:
(define grid (make-grid 10 10 
                       (lambda (x y) (+ (* x x) (* y y)))))
```

### Multiple Output Formats

Save the same plot to multiple formats:

```scheme
(define (save-plot-all-formats basename)
  (define (make-plot device ext)
    (plot-device device)
    (plot-output (string-append basename ext))
    (plot-init)
    ; ... your plotting code here ...
    (plot-end))
  
  (make-plot "pdfqt" ".pdf")
  (make-plot "svgqt" ".svg")
  (make-plot "pngqt" ".png"))
```

### Custom Color Maps

Create a gradient effect:

```scheme
(define (plot-gradient)
  (plot-device "qtwidget")
  (plot-init)
  
  (plot-env 0.0 10.0 0.0 10.0)
  
  ; Draw lines with increasing color
  (let loop ((i 0))
    (when (< i 100)
      (let ((x (/ i 10.0))
            (color (inexact->exact (truncate (* 15 (/ i 100))))))
        (plot-color color)
        (plot-line (list x x) '(0.0 10.0))
        (loop (+ i 1)))))
  
  (plot-end))
```

---

## Troubleshooting

### Common Issues

#### 1. "Cannot find plplot library"

**Problem:** PLplot not installed or not in pkg-config path.

**Solution:**
```bash
# Ubuntu
sudo apt install libplplot-dev plplot-driver-qt

# macOS
brew install plplot

# Verify
pkg-config --exists plplot && echo "Found" || echo "Not found"
```

#### 2. Qt window doesn't appear

**Problem:** Qt driver not installed.

**Solution:**
```bash
# Ubuntu
sudo apt install plplot-driver-qt

# Verify available devices
plplot -h  # Or check /usr/lib/plplot<version>/drivers/
```

#### 3. Arrays of different lengths

**Error:** "x and y must have same length"

**Solution:** Ensure all coordinate lists have the same number of elements:
```scheme
(length x-list)  ; Should equal
(length y-list)  ; Should equal
```

#### 4. PDF file not created

**Problem:** Output filename not set or write permissions.

**Solution:**
```scheme
(plot-device "pdfqt")
(plot-output "output.pdf")  ; Must specify filename!
(plot-init)
; ...
(plot-end)
```

#### 5. Plot appears blank

**Problem:** Axis ranges don't include data.

**Solution:** Check that `plot-env` ranges encompass your data:
```scheme
; If data ranges from 0-100
(plot-env 0.0 100.0 0.0 100.0)  ; Good
(plot-env 0.0 10.0 0.0 10.0)    ; Bad - data out of range
```

### Getting Help

1. Check PLplot version:
   ```scheme
   (plot-version)
   ```

2. Verify test suite passes:
   ```scheme
   (load "test-plplot.scm")
   (run-all-tests)
   ```

3. Try minimal example to isolate issue

4. Check PLplot documentation: http://plplot.org/documentation.php

---

## Performance Tips

1. **Large datasets:** PLplot handles arrays efficiently, but very large datasets (>10,000 points) may be slow. Consider sampling data.

2. **Multiple plots:** Reuse data instead of regenerating:
   ```scheme
   (define data (expensive-calculation))
   (plot-1 data)
   (plot-2 data)
   ```

3. **PDF vs Qt:** PDF generation is generally faster than interactive Qt windows for batch plotting.

4. **Nulldriver for testing:** Use `"nulldriver"` device for performance testing without actual rendering.

---

## See Also

- [PLplot official documentation](http://plplot.org/documentation.php)
- [SIOD-TR Raylib integration](raylib-examples.scm) - For interactive graphics
- [SIOD-TR Quaternion library](quaternion-examples.scm) - For 3D mathematics
- [PLplot examples](http://plplot.org/examples.php) - Official PLplot examples

---

**SIOD-TR PLplot Integration**  
Version 1.0  
License: LGPL (matching PLplot)
