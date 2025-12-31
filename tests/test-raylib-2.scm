;;; Phase 2 Simple Test
;;; Tests render texture and camera basics

(require-so (so-ext 'raylib))

(define modulo %)

;;; Define defined? for SIOD compatibility
(define (defined? sym)
  "Check if a symbol is defined"
  (symbol-bound? sym (the-environment)))

;;; Test 1: Render Texture Basics
(define (test-render-texture)
  (display "Test 1: Render Texture")
  (newline)
  
  (init-window 400 300 "Test: Render Texture")
  (set-target-fps 60)
  
  ;; Create render texture
  (create-render-texture 400 300)
  
  ;; Draw to texture once
  (display "Drawing to texture...")
  (newline)
  (begin-texture-mode)
    (clear-background 'raywhite)
    (draw-circle 200 150 50 'red)
    (draw-rectangle 100 100 100 50 'blue)
    (draw-text "Rendered to texture!" 50 250 20 'darkgray)
  (end-texture-mode)
  (display "Done!")
  (newline)
  
  ;; Display texture repeatedly
  (let ((frame 0))
    (while (not (window-should-close?))
      (begin-drawing)
        (clear-background 'black)
        ;; Draw the texture - instant!
        (draw-render-texture 0 0 'white)
        ;; Overlay showing this is live
        (draw-text (string-append "Frame: " (number->string frame))
                  10 10 15 'green)
      (end-drawing)
      (set! frame (+ frame 1))))
  
  (unload-render-texture)
  (close-window)
  (display "Test 1: PASSED")
  (newline))

;;; Test 2: Camera Basics
(define (test-camera)
  (display "Test 2: Camera Controls")
  (newline)
  
  (init-window 400 300 "Test: Camera - Use Arrows")
  (set-target-fps 60)
  
  ;; Initialize camera
  (init-camera 200 150 1.0)
  
  (while (not (window-should-close?))
    ;; Camera controls
    (if (key-down? KEY_UP)    (camera-move 0 -5))
    (if (key-down? KEY_DOWN)  (camera-move 0 5))
    (if (key-down? KEY_LEFT)  (camera-move -5 0))
    (if (key-down? KEY_RIGHT) (camera-move 5 0))
    (if (key-down? KEY_EQUAL) (camera-zoom-by 1.1))
    (if (key-down? KEY_MINUS) (camera-zoom-by 0.9))
    
    (begin-drawing)
      (clear-background 'raywhite)
      
      ;; Draw with camera
      (begin-camera-mode)
        ;; Draw grid to show camera movement
        (let ((x -400))
          (while (< x 800)
            (draw-line x -300 x 600 'lightgray)
            (set! x (+ x 50))))
        (let ((y -300))
          (while (< y 600)
            (draw-line -400 y 800 y 'lightgray)
            (set! y (+ y 50))))
        
        ;; Origin marker
        (draw-circle 0 0 10 'red)
        (draw-text "ORIGIN" 15 -5 20 'red)
        
        ;; Reference objects
        (draw-rectangle -50 -50 100 100 'blue)
        (draw-circle 200 100 30 'green)
      (end-camera-mode)
      
      ;; UI (not affected by camera)
      (draw-text "Arrows: Move | +/-: Zoom" 10 10 15 'black)
      (let ((zoom (camera-get-zoom)))
        (draw-text (string-append "Zoom: " 
                                  (number->string (floor (* zoom 100)))
                                  "%")
                  10 30 15 'black))
    (end-drawing))
  
  (close-window)
  (display "Test 2: PASSED")
  (newline))

;;; Test 3: Combined - Texture with Camera
(define (test-combined)
  (display "Test 3: Texture + Camera")
  (newline)
  
  (init-window 400 300 "Test: Combined - Use Arrows")
  (set-target-fps 60)
  
  ;; Create and render to texture
  (create-render-texture 400 300)
  (begin-texture-mode)
    (clear-background 'black)
    (draw-circle 200 150 80 'red)
    (draw-circle 100 100 50 'blue)
    (draw-circle 300 200 60 'green)
    (draw-text "Texture Content" 120 140 20 'white)
  (end-texture-mode)
  
  ;; Initialize camera
  (init-camera 200 150 1.0)
  
  (while (not (window-should-close?))
    ;; Camera controls
    (if (key-down? KEY_UP)    (camera-move 0 -5))
    (if (key-down? KEY_DOWN)  (camera-move 0 5))
    (if (key-down? KEY_LEFT)  (camera-move -5 0))
    (if (key-down? KEY_RIGHT) (camera-move 5 0))
    (if (key-down? KEY_EQUAL) (camera-zoom-by 1.05))
    (if (key-down? KEY_MINUS) (camera-zoom-by 0.95))
    
    (begin-drawing)
      (clear-background 'darkgray)
      
      ;; Draw texture with camera
      (begin-camera-mode)
        (draw-render-texture 0 0 'white)
      (end-camera-mode)
      
      ;; UI
      (draw-text "Arrows: Pan | +/-: Zoom" 10 10 15 'yellow)
      (draw-text (string-append "Zoom: " 
                                (number->string (floor (* (camera-get-zoom) 100)))
                                "%")
                10 30 15 'yellow)
    (end-drawing))
  
  (unload-render-texture)
  (close-window)
  (display "Test 3: PASSED")
  (newline))

;;; Run all tests
(define (test-all)
  (display "=== Phase 2 Tests ===")
  (newline)
  (test-render-texture)
  (test-camera)
  (test-combined)
  (display "=== All Tests Complete ===")
  (newline))

;;; Helper
(if (not (defined? 'string-append))
    (define (string-append . strings)
      (if (null? strings)
          ""
          (if (null? (cdr strings))
              (car strings)
              (string-concat (car strings)
                           (apply string-append (cdr strings)))))))

(display "Phase 2 Tests Loaded")
(newline)
(display "Run: (test-all) or individual tests:")
(newline)
(display "  (test-render-texture)")
(newline)
(display "  (test-camera)")
(newline)
(display "  (test-combined)")
(newline)
