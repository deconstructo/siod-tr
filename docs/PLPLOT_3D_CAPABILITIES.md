# PLplot 3D Plotting Capabilities - YES!

## Confirmation: PLplot Fully Supports 3D Plotting

PLplot has excellent 3D plotting capabilities including:

- ✅ **3D Line Plots** - Draw lines in 3D space
- ✅ **3D Surface Plots** - Colored/shaded surfaces
- ✅ **3D Mesh Plots** - Wireframe meshes
- ✅ **3D Contour Plots** - Contours on surfaces
- ✅ **Adjustable viewing angles** - Altitude and azimuth control
- ✅ **Axis labels and boxes** - Full 3D coordinate system

## 3D Functions in SIOD-TR Integration

### Core 3D Functions

1. **`(plot-3d-init xmin xmax ymin ymax zmin zmax altitude azimuth)`**
   - Sets up 3D plot environment
   - `altitude`: viewing angle above XY plane (0-90°)
   - `azimuth`: rotation around Z axis (0-360°)

2. **`(plot-3d-box xlabel ylabel zlabel)`**
   - Draws 3D coordinate box with labels

3. **`(plot-3d-line x-list y-list z-list)`**
   - Draws line connecting 3D points

4. **`(plot-3d-surface x-list y-list z-grid)`**
   - Draws colored 3D surface
   - z-grid is list of lists (2D array)

5. **`(plot-3d-mesh x-list y-list z-grid)`**
   - Draws 3D wireframe mesh

## Simple 3D Example

```scheme
; 3D helix
(define (plot-3d-helix)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Create helix points
  (define (range start end step)
    (let loop ((t start) (acc '()))
      (if (> t end)
          (reverse acc)
          (loop (+ t step) (cons t acc)))))
  
  (let* ((t (range 0.0 12.56 0.2))
         (x (map cos t))
         (y (map sin t))
         (z (map (lambda (v) (* v 0.1)) t)))
    
    (plot-3d-init -1.5 1.5 -1.5 1.5 0.0 1.5 30.0 45.0)
    (plot-3d-box "X" "Y" "Z")
    
    (plot-color 2)
    (plot-width 2.0)
    (plot-3d-line x y z))
  
  (plot-end))
```

## 3D Surface Example

```scheme
; Plot z = sin(x) * cos(y)
(define (plot-wave-surface)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Create grid
  (let ((x '(-3.0 -2.0 -1.0 0.0 1.0 2.0 3.0))
        (y '(-3.0 -2.0 -1.0 0.0 1.0 2.0 3.0)))
    
    ; Compute z = sin(x) * cos(y) for each grid point
    (define (compute-z xi yi)
      (* (sin xi) (cos yi)))
    
    ; Build z grid (7 rows × 7 columns)
    (define (build-row yi)
      (map (lambda (xi) (compute-z xi yi)) x))
    
    (let ((z (map build-row y)))
      
      (plot-3d-init -3.0 3.0 -3.0 3.0 -1.0 1.0 30.0 45.0)
      (plot-3d-box "X" "Y" "sin(X)cos(Y)")
      
      (plot-color 4)
      (plot-3d-surface x y z)))
  
  (plot-end))
```

## Viewing Angles

Different perspectives of the same surface:

```scheme
; View from above
(plot-3d-init -1.0 1.0 -1.0 1.0 0.0 1.0 70.0 0.0)

; View from side
(plot-3d-init -1.0 1.0 -1.0 1.0 0.0 1.0 10.0 90.0)

; Isometric view
(plot-3d-init -1.0 1.0 -1.0 1.0 0.0 1.0 35.264 45.0)

; Default nice view
(plot-3d-init -1.0 1.0 -1.0 1.0 0.0 1.0 30.0 45.0)
```

## 3D Plot Types Compared

### 3D Line Plot
- Good for: Trajectories, paths, curves in space
- Example: Particle trajectory, flight path, quaternion rotation sequence

### 3D Surface Plot  
- Good for: Functions z = f(x,y), terrain data, potential fields
- Example: Mandelbrot set height map, probability distributions

### 3D Mesh Plot
- Good for: Wireframe visualization, seeing through surfaces
- Example: Mathematical surfaces, finite element meshes

## Real-World 3D Examples

### Quaternion Rotation Visualization

```scheme
; Visualize quaternion rotation in 3D
(define (plot-quaternion-rotation)
  (plot-device "qtwidget")
  (plot-init)
  
  ; Rotate point (1,0,0) around axis (1,1,1)
  (define axis '(1.0 1.0 1.0))
  
  (define (range start end step)
    (let loop ((t start) (acc '()))
      (if (> t end)
          (reverse acc)
          (loop (+ t step) (cons t acc)))))
  
  (let* ((angles (range 0.0 6.28 0.1))
         (quats (map (lambda (a) (q-from-axis-angle axis a)) angles))
         (point '(1.0 0.0 0.0))
         (rotated-points (map (lambda (q) (q-rotate point q)) quats)))
    
    ; Extract x, y, z coordinates
    (let ((x (map car rotated-points))
          (y (map cadr rotated-points))
          (z (map caddr rotated-points)))
      
      (plot-3d-init -1.5 1.5 -1.5 1.5 -1.5 1.5 30.0 45.0)
      (plot-3d-box "X" "Y" "Z")
      
      (plot-color 2)
      (plot-width 2.0)
      (plot-3d-line x y z)))
  
  (plot-end))
```

### Mandelbrot Escape-Time 3D Visualization

```scheme
; Plot Mandelbrot iteration count as height
(define (plot-mandelbrot-3d)
  (plot-device "qtwidget")
  (plot-init)
  
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
  
  ; Sample Mandelbrot on a grid
  (let ((x '(-2.0 -1.5 -1.0 -0.5 0.0 0.5 1.0))
        (y '(-1.0 -0.5 0.0 0.5 1.0)))
    
    ; Compute iteration counts
    (define (compute-row yi)
      (map (lambda (xi) (mandelbrot-iter xi yi 50)) x))
    
    (let ((z (map compute-row y)))
      
      (plot-3d-init -2.0 1.0 -1.0 1.0 0.0 50.0 30.0 45.0)
      (plot-3d-box "Real" "Imaginary" "Iterations")
      
      (plot-color 3)
      (plot-3d-surface x y z)))
  
  (plot-end))
```

## Advanced: Parametric Surfaces

Create complex 3D shapes:

```scheme
; Torus (donut shape)
(define (plot-torus)
  (plot-device "qtwidget")
  (plot-init)
  
  (define R 2.0)  ; Major radius
  (define r 0.5)  ; Minor radius
  
  ; Parametric equations:
  ; x = (R + r*cos(v)) * cos(u)
  ; y = (R + r*cos(v)) * sin(u)
  ; z = r * sin(v)
  
  (define (torus-x u v)
    (* (+ R (* r (cos v))) (cos u)))
  
  (define (torus-y u v)
    (* (+ R (* r (cos v))) (sin u)))
  
  (define (torus-z u v)
    (* r (sin v)))
  
  ; Sample u and v
  (let ((u-vals '(0.0 1.57 3.14 4.71 6.28))
        (v-vals '(0.0 1.57 3.14 4.71 6.28)))
    
    ; Build coordinate grids
    (define (build-grid func)
      (map (lambda (v)
             (map (lambda (u) (func u v)) u-vals))
           v-vals))
    
    (let ((x (build-grid torus-x))
          (y (build-grid torus-y))
          (z (build-grid torus-z)))
      
      (plot-3d-init -3.0 3.0 -3.0 3.0 -1.0 1.0 30.0 45.0)
      (plot-3d-box "X" "Y" "Z")
      
      (plot-color 4)
      (plot-3d-mesh (car x) (car y) z)))
  
  (plot-end))
```

## Performance Notes

3D plotting is computationally intensive. For smooth rendering:

- **Interactive Qt**: 20×20 grid is smooth, 50×50 is usable
- **PDF output**: Can handle larger grids (100×100+)
- **Mesh vs Surface**: Mesh is faster than shaded surfaces

## Summary

**Yes, PLplot does excellent 3D plotting!**

The SIOD-TR integration provides:
- Complete 3D coordinate system
- Multiple plot types (line, surface, mesh)
- Viewing angle control
- Integration with quaternion library for rotation visualization
- Perfect for mathematical surfaces, trajectories, and scientific data

All the 3D functions are fully implemented in `siod_plplot.c` and documented in the full documentation.
