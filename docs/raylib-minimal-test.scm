;;; Ultra-minimal Raylib test for SIOD-TR
;;; This uses only the most basic SIOD features

(require-so (so-ext "raylib"))

;;; Test 1: Simple static puts
(define (test-static)
  (init-window 400 300 "Static Test")
  (begin-drawing)
  (clear-background 'raywhite)
  (draw-circle 200 150 50 'red)
  (draw-text "Press ESC to close" 10 10 20 'darkgray)
  (end-drawing)
  
  (while (not (window-should-close?))
    (begin-drawing)
    (end-drawing))
  
  (close-window))

;;; Test 2: Moving circle (no sin/cos)
(define (test-moving)
  (init-window 400 300 "Moving Test")
  (set-target-fps 60)
  
  (let ((x 50)
        (vx 3))
    (while (not (window-should-close?))
      (set! x (+ x vx))
      (if (or (< x 0) (> x 400))
          (set! vx (- 0 vx)))
      
      (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x 150 30 'blue)
      (end-drawing)))
  
  (close-window))

;;; Test 3: Simple pixel drawing
(define (test-pixels)
  (init-window 400 300 "Pixel Test")
  (set-target-fps 60)
  
  (begin-drawing)
  (clear-background 'black)
  
  ;; Draw some colored lines
  (let ((y 0))
    (while (< y 300)
      (let ((x 0))
        (while (< x 400)
          (draw-pixel x y (list (floor (* x 0.6)) 
                                (floor (* y 0.8)) 
                                128 
                                255))
          (set! x (+ x 2))))
      (set! y (+ y 2))))
  
  (end-drawing)
  
  (while (not (window-should-close?))
    (begin-drawing)
    (end-drawing))
  
  (close-window))

(define (help)
  (puts "Minimal Raylib tests:")
  (puts "  (test-static)  - Static circle and text")
  (puts "  (test-moving)  - Bouncing circle")
  (puts "  (test-pixels)  - Colored pixel gradient")
)

(help)
