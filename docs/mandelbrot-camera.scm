;;; SIOD-TR Raylib Phase 2 Example
;;; Optimized Mandelbrot with Camera Controls
;;;
;;; Features:
;;; - Renders Mandelbrot to texture ONCE (slow)
;;; - Displays texture at 60 FPS (instant!)
;;; - Arrow keys to pan
;;; - +/- or mouse wheel to zoom
;;;
;;; Load: (load "mandelbrot-camera.scm")
;;; Run: (mandelbrot-camera)

(require-so (so-ext 'raylib))

;;; Define defined? for SIOD compatibility
(define (defined? sym)
  "Check if a symbol is defined"
  (symbol-bound? sym (the-environment)))

;;; Helper functions from original
(define (map-range value in-min in-max out-min out-max)
  (+ out-min 
     (* (- out-max out-min)
        (/ (- value in-min) (- in-max in-min)))))

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

(define (iter->color iters max-iter)
  (if (>= iters max-iter)
      '(0 0 0 255)
      (let ((t (/ iters max-iter)))
        (list (floor (* t 255))
              (floor (* t 200))
              255
              255))))

;;; Render Mandelbrot to the global render texture
(define (render-mandelbrot width height)
  (define max-iter 100)
  (define x-min -2.5)
  (define x-max 1.0)
  (define y-min -1.0)
  (define y-max 1.0)
  
  ;; Try to reduce GC noise (SIOD-specific)
  (if (defined? 'verbose) (verbose 0))
  (if (defined? 'gc-status) (gc-status #f))
  
  (display "Rendering Mandelbrot to texture...")
  (newline)
  
  (begin-texture-mode)
    (clear-background 'black)
    
    ;; Render all pixels - only happens once!
    (let ((x 0))
      (while (< x width)
        ;; Progress indicator every 50 columns
        (if (= (modulo x 50) 0)
            (begin
              (display ".")
              (if (= (modulo x 200) 0)
                  (begin
                    (display " ")
                    (display (floor (* 100 (/ x width))))
                    (display "%")
                    (newline)))))
        
        (let ((y 0))
          (while (< y height)
            (let* ((cx (map-range x 0 width x-min x-max))
                   (cy (map-range y 0 height y-min y-max))
                   (iters (mandelbrot-iter cx cy max-iter))
                   (color (iter->color iters max-iter)))
              (draw-pixel x y color))
            (set! y (+ y 1))))
        (set! x (+ x 1))))
  (end-texture-mode)
  
  (display "Done!")
  (newline))

;;; Main program
(define (mandelbrot-camera)
  (define width 800)
  (define height 600)
  
  (init-window width height "Mandelbrot Explorer - Arrows: Pan | +/-: Zoom")
  (set-target-fps 60)
  
  ;; Create render texture
  (create-render-texture width height)
  
  ;; Render Mandelbrot once
  (render-mandelbrot width height)
  
  ;; Initialize camera at center, zoom 1.0
  (init-camera (/ width 2) (/ height 2) 1.0)
  
  ;; Movement speed
  (define pan-speed 10)
  (define zoom-speed 0.1)
  
  ;; Main loop - displays at 60 FPS!
  (while (not (window-should-close?))
    
    ;; Handle input - pan with arrows
    (if (key-down? KEY_UP)    (camera-move 0 (- 0 pan-speed)))
    (if (key-down? KEY_DOWN)  (camera-move 0 pan-speed))
    (if (key-down? KEY_LEFT)  (camera-move (- 0 pan-speed) 0))
    (if (key-down? KEY_RIGHT) (camera-move pan-speed 0))
    
    ;; Zoom with +/- keys
    (if (key-down? KEY_EQUAL) (camera-zoom-by (+ 1.0 zoom-speed)))
    (if (key-down? KEY_MINUS) (camera-zoom-by (- 1.0 zoom-speed)))
    
    ;; Draw
    (begin-drawing)
      (clear-background 'black)
      
      ;; Draw with camera transformation
      (begin-camera-mode)
        (draw-render-texture 0 0 'white)
      (end-camera-mode)
      
      ;; UI overlay (not affected by camera)
      (draw-text "Arrow Keys: Pan" 10 10 20 'white)
      (draw-text "+/- Keys: Zoom" 10 35 20 'white)
      
      ;; Show camera info
      (let ((zoom (camera-get-zoom)))
        (draw-text (string-append "Zoom: " 
                                  (number->string (floor (* zoom 100)))
                                  "%")
                  10 (- height 25) 20 'green))
      
      (draw-text (string-append "FPS: " (number->string (get-fps)))
                10 (- height 50) 20 'green)
    (end-drawing))
  
  ;; Cleanup
  (unload-render-texture)
  (close-window))

;;; Helper for string concatenation if missing
(if (not (defined? 'string-append))
    (define (string-append . strings)
      (if (null? strings)
          ""
          (if (null? (cdr strings))
              (car strings)
              (string-concat (car strings)
                           (apply string-append (cdr strings)))))))

(display "Mandelbrot Camera Explorer loaded!")
(newline)
(display "Run: (mandelbrot-camera)")
(newline)
