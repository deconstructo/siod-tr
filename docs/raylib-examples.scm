;;; SIOD-TR Raylib Examples
;;; Phase 1: Hello World and Mandelbrot
;;;
;;; To use these examples:
;;; 1. Load this file: (load "raylib-examples.scm")
;;; 2. Start simple:  (raylib-test)  ; Animated circle - tests basics
;;; 3. Try examples:  (hello-world-1)  ; Bouncing ball
;;; 4. Go complex:    (mandelbrot-static)  ; The Mandelbrot set!
;;;
;;; Note: These examples use 'while' loops (no 'do' macro needed)

(require-so (so-ext "raylib"))

(define modulo %)

;;; Example 1: Basic Hello World - Bouncing Ball

(define (hello-world-1)
  (init-window 800 600 "SIOD-TR: Hello Raylib")
  (set-target-fps 60)
  
  (let ((x 400)
        (y 300)
        (vx 3)
        (vy 2))
    
    (while (not (window-should-close?))
      ;; Update
      (set! x (+ x vx))
      (set! y (+ y vy))
      
      ;; Bounce off edges
      (if (or (< x 0) (> x 800))
        (set! vx (- 0 vx)))
      (if (or (< y 0) (> y 600))
        (set! vy (- 0 vy)))
      
      ;; Draw
      (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x y 20 'red)
      (draw-text "Hello SIOD-TR!" 10 10 20 'darkgray)
      (end-drawing)))
  
  (close-window))

;;; Example 2: Interactive - Keyboard Control
(define (hello-world-interactive)
  (init-window 800 600 "SIOD-TR: Arrow Keys to Move")
  (set-target-fps 60)
  
  (let ((x 400)
        (y 300)
        (speed 5))
    
    (while (not (window-should-close?))
      ;; Input
      (if (key-down? KEY_RIGHT) (set! x (+ x speed)))
      (if (key-down? KEY_LEFT)  (set! x (- x speed)))
      (if (key-down? KEY_DOWN)  (set! y (+ y speed)))
      (if (key-down? KEY_UP)    (set! y (- y speed)))
      
      ;; Draw
      (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x y 25 'blue)
      (draw-text "Use Arrow Keys" 10 10 20 'darkgray)
      (end-drawing)))
  
  (close-window))

;;; Example 3: Mouse Tracking
(define (hello-world-mouse)
  (init-window 800 600 "SIOD-TR: Click to Draw")
  (set-target-fps 60)
  
  (while (not (window-should-close?))
    (let ((pos (mouse-position)))
      (begin-drawing)
      (clear-background 'raywhite)
      
      ;; Draw circle at mouse position
      (draw-circle (car pos) (cdr pos) 30 'green)
      
      ;; Draw line from center to mouse
      (draw-line 400 300 (car pos) (cdr pos) 'red)
      
      (draw-text "Move Your Mouse" 10 10 20 'darkgray)
      (end-drawing)))
  
  (close-window))

;;; ============================================
;;; MANDELBROT SET - Hello World 2
;;; ============================================

;;; Helper: Map value from one range to another
(define (map-range value in-min in-max out-min out-max)
  (+ out-min 
     (* (- out-max out-min)
        (/ (- value in-min) (- in-max in-min)))))

;;; Calculate Mandelbrot iterations for point (cx, cy)
(define (mandelbrot-iter cx cy max-iter)
  (define (iter-loop zx zy iter)
    (let ((zx2 (* zx zx))
          (zy2 (* zy zy)))
      (if (or (>= iter max-iter)
              (> (+ zx2 zy2) 4.0))
          iter
          (iter-loop (+ (- zx2 zy2) cx)
                     (+ (* 2.0 zx zy) cy)
                     (+ iter 1)))))
  (iter-loop 0.0 0.0 0))

;;; Map iteration count to color
(define (iter->color iters max-iter)
  (if (>= iters max-iter)
      '(0 0 0 255)  ;; Black for inside set
      (let ((t (/ iters max-iter)))
        ;; Simple gradient: blue -> cyan -> white
        (list (floor (* t 255))
              (floor (* t 200))
              255
              255))))

;;; Example 4: Static Mandelbrot (renders progressively)
;;; Renders 10 columns per frame for smooth progressive display
(define (mandelbrot-static)
  (let ((width 800)
        (height 600)
        (max-iter 100)
        (x-min -2.5)
        (x-max 1.0)
        (y-min -1.0)
        (y-max 1.0)
        (cols-per-frame 10))  ; Render 10 columns per frame
    
    (init-window width height "SIOD-TR: Mandelbrot Set")
    (set-target-fps 60)
    
    ;; Clear once at the start
    (begin-drawing)
    (clear-background 'black)
    (draw-text "Rendering..." 10 10 20 'white)
    (end-drawing)
    
    ;; Render Mandelbrot in batches
    (let ((x 0))
      (while (< x width)
        (begin-drawing)
        ;; Don't clear! We want pixels to accumulate
        
        ;; Render cols-per-frame columns this frame
        (let ((batch-end (min (+ x cols-per-frame) width)))
          (let ((col x))
            (while (< col batch-end)
              (let ((y 0))
                (while (< y height)
                  (let* ((cx (map-range col 0 width x-min x-max))
                         (cy (map-range y 0 height y-min y-max))
                         (iters (mandelbrot-iter cx cy max-iter))
                         (color (iter->color iters max-iter)))
                    (draw-pixel col y color))
                  (set! y (+ y 1))))
              (set! col (+ col 1)))))
        
        ;; Show simple progress
        (draw-text "Rendering Mandelbrot..." 10 10 20 'white)
        (end-drawing)
        
        (set! x (+ x cols-per-frame))))
    
    ;; Display final result
    (while (not (window-should-close?))
      (begin-drawing)
      ;; Don't clear! Keep the mandelbrot
      (draw-text "Mandelbrot Set - Press ESC to exit" 10 10 20 'white)
      (end-drawing))
    
    (close-window)))

;;; Simple test - just draw some shapes (good first test!)
(define (raylib-test)
  (init-window 400 300 "SIOD-TR Raylib Test")
  (set-target-fps 60)
  
  (let ((frame 0))
    (while (not (window-should-close?))
      (begin-drawing)
      (clear-background 'raywhite)
      (draw-text "Raylib works!" 100 50 30 'darkgray)
      (draw-circle 200 150 (+ 40 (* 10 (sin (* frame 0.05)))) 'red)
      (draw-rectangle 150 200 100 50 'blue)
      (end-drawing)
      (set! frame (+ frame 1))))
  
  (close-window))

;;; Example 5: Interactive Mandelbrot (zoom/pan with arrow keys)
;;; This redraws on key press - slow but workable for Phase 1
(define (mandelbrot-interactive)
  (let ((width 800)
        (height 600)
        (max-iter 100))
    
    (init-window width height "SIOD-TR: Mandelbrot - Arrow Keys to Pan")
    (set-target-fps 60)
    
    (let ((x-min -2.5)
          (x-max 1.0)
          (y-min -1.0)
          (y-max 1.0)
          (need-redraw #t))
      
      (define (render-mandelbrot)
        (begin-drawing)
        (clear-background 'black)
        
        (let ((x 0))
          (while (< x width)
            (let ((y 0))
              (while (< y height)
                (let* ((cx (map-range x 0 width x-min x-max))
                       (cy (map-range y 0 height y-min y-max))
                       (iters (mandelbrot-iter cx cy max-iter))
                       (color (iter->color iters max-iter)))
                  (draw-pixel x y color))
                (set! y (+ y 1))))
            (set! x (+ x 1))))
        
        (draw-text "Arrow keys: Pan | +/-: Zoom | ESC: Exit" 10 10 20 'white)
        (end-drawing))
      
      (define (pan-zoom!)
        (let ((pan-factor 0.1)
              (zoom-factor 0.8))
          (let ((x-range (- x-max x-min))
                (y-range (- y-max y-min)))
            
            ;; Pan
            (if (key-pressed? KEY_RIGHT)
              (set! x-min (+ x-min (* x-range pan-factor)))
              (set! x-max (+ x-max (* x-range pan-factor)))
              (set! need-redraw #t))
            (if (key-pressed? KEY_LEFT)
              (set! x-min (- x-min (* x-range pan-factor)))
              (set! x-max (- x-max (* x-range pan-factor)))
              (set! need-redraw #t))
            (if (key-pressed? KEY_UP)
              (set! y-min (- y-min (* y-range pan-factor)))
              (set! y-max (- y-max (* y-range pan-factor)))
              (set! need-redraw #t))
            (if (key-pressed? KEY_DOWN)
              (set! y-min (+ y-min (* y-range pan-factor)))
              (set! y-max (+ y-max (* y-range pan-factor)))
              (set! need-redraw #t))
            
            ;; Zoom (not implemented yet - would need +/- key constants)
            )))
      
      ;; Main loop
      (while (not (window-should-close?))
        (pan-zoom!)
        
        (if need-redraw
            (begin
              (render-mandelbrot)
              (set! need-redraw #f))
            ;; Just keep displaying current frame
            (begin
              (begin-drawing)
              (end-drawing))))
      
      (close-window))))

;;; To run examples:
;;; (load "raylib-examples.scm")
;;; (raylib-test)              ; Simple animated test - START HERE!
;;; (hello-world-1)            ; Bouncing ball
;;; (hello-world-interactive)  ; Keyboard-controlled circle
;;; (hello-world-mouse)        ; Mouse tracking
;;; (mandelbrot-static)        ; The Mandelbrot set! (progressive render)
;;; (mandelbrot-interactive)   ; Pan with arrow keys (slow but workable)
