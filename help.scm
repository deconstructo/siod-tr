;;; SIOD-TR Help System
;;; Discoverable API documentation
;;; Load: (load "help.scm")

;;; ============================================
;;; Compatibility Fixes
;;; ============================================

(define (display str)
        (puts str))

(define (newline) (display "\n"))

;;; ============================================
;;; Help Database
;;; ============================================

;;; Help database - extend this!
(define *help-db* '())

(define (add-help name args description)
  "Add help entry for a function"
  (set! *help-db*
        (cons (list name args description)
              *help-db*)))

;;; Register built-in functions
(add-help 'car '(pair) "Returns the first element of a pair")
(add-help 'cdr '(pair) "Returns the rest of the list")
(add-help 'cons '(obj list) "Constructs a new pair")
(add-help 'list '(obj ...) "Creates a list from arguments")
(add-help 'append '(list1 list2) "Concatenates two lists")
(add-help 'length '(list) "Returns the length of a list")
(add-help 'reverse '(list) "Reverses a list")
(add-help 'map '(proc list) "Applies proc to each element")

;;; Math functions
(add-help 'sin '(x) "Sine of x (radians)")
(add-help 'cos '(x) "Cosine of x (radians)")
(add-help 'tan '(x) "Tangent of x (radians)")
(add-help 'sqrt '(x) "Square root of x")
(add-help 'exp '(x) "e^x")
(add-help 'log '(x) "Natural logarithm of x")
(add-help 'floor '(x) "Round down to nearest integer")
(add-help 'ceiling '(x) "Round up to nearest integer")
(add-help 'abs '(x) "Absolute value of x")

;;; Raylib functions
(add-help 'init-window '(width height title) "Create a window")
(add-help 'close-window '() "Close the window")
(add-help 'begin-drawing '() "Begin drawing frame")
(add-help 'end-drawing '() "End drawing frame")
(add-help 'clear-background '(color) "Clear screen with color")
(add-help 'draw-circle '(x y radius color) "Draw a circle")
(add-help 'draw-rectangle '(x y width height color) "Draw a rectangle")
(add-help 'draw-pixel '(x y color) "Draw a single pixel")
(add-help 'draw-text '(text x y size color) "Draw text")
(add-help 'window-should-close? '() "Check if window should close")

;;; QoL functions
(add-help 'range '(start end) "Generate list of numbers from start to end")
(add-help 'iota '(n) "Generate list (0 1 2 ... n-1)")
(add-help 'filter '(pred list) "Keep elements where pred is true")
(add-help 'fold '(proc init list) "Fold list with proc")
(add-help 'sum '(list) "Sum all numbers in list")
(add-help 'time '(expr) "Time execution of expression")

(define (help name)
  "Display help for a function"
  (let ((entry (assq name *help-db*)))
    (if entry
        (begin
          (display "(")(display name)(display " ")
          (display (cadr entry))(display ")")
          (newline)
          (display "  ")(display (caddr entry))
          (newline))
        (begin
          (display "No help available for: ")
          (display name)
          (newline)
          (display "Try: (apro \"keyword\")")
          (newline)))))

(define (apro keyword)
  "Find functions matching keyword"
  (display "Functions matching '")
  (display keyword)
  (display "':")
  (newline)
  (let loop ((db *help-db*))
    (if (null? db)
        #t
        (let ((entry (car db)))
          (let ((name (symbol->string (car entry))))
            (if (string-contains? name keyword)
                (begin
                  (display "  ")
                  (display (car entry))
                  (display " - ")
                  (display (caddr entry))
                  (newline))))
          (loop (cdr db))))))

(define (string-contains? str substr)
  "Check if str contains substr (simple version)"
  ;; This is a placeholder - actual implementation depends
  ;; on SIOD's string functions
  #f)

(define (help-topics)
  "List all available help topics"
  (display "Available help topics:")
  (newline)
  (let loop ((db *help-db*))
    (if (null? db)
        #t
        (begin
          (display "  ")
          (display (car (car db)))
          (newline)
          (loop (cdr db))))))

(define (help-summary)
  "Show help system usage"
  (display "SIOD-TR Help System")
  (newline)
  (display "  (help 'function-name)  - Show help for function")
  (newline)
  (display "  (apropos \"keyword\")    - Search for functions")
  (newline)
  (display "  (help-topics)          - List all topics")
  (newline)
  (display "  (help-summary)         - This message")
  (newline))

;;; Startup message
(display "Help system loaded. Try: (help 'car) or (help-summary)")
(newline)
