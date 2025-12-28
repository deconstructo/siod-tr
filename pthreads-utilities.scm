;;; pthreads-utilities.scm - High-level threading patterns for SIOD
;;;
;;; Since SIOD's evaluator isn't thread-safe, we provide coordination
;;; primitives rather than trying to run Scheme code in threads.
;;;
;;; Use mutexes and condition variables to coordinate between:
;;; - Main Scheme thread (does evaluation)
;;; - Worker threads (do C-level computation via extensions)

(require-so (so-ext "pthreads"))

(puts "PTHREADS NOTE: SIOD's evaluator is not thread-safe.\n")
(puts "Use mutexes/condvars for coordination, not pthread-create for Scheme code.\n")
(puts "For parallel computation, use task-based patterns or external C functions.\n")

;;; ============================================================================
;;; Safe Patterns
;;; ============================================================================

;; Example: Mutex-protected counter
(define (make-counter)
  (let ((value 0)
        (m (mutex-create)))
    (lambda (op . args)
      (cond 
        ((eq? op 'inc)
         (mutex-lock m)
         (set! value (+ value 1))
         (mutex-unlock m)
         value)
        ((eq? op 'dec)
         (mutex-lock m)
         (set! value (- value 1))
         (mutex-unlock m)
         value)
        ((eq? op 'get)
         (mutex-lock m)
         (define v value)
         (mutex-unlock m)
         v)))))

;; Example: Simple semaphore
(define (make-semaphore initial)
  (let ((count initial)
        (m (mutex-create))
        (c (cond-create)))
    (lambda (op)
      (cond
        ((eq? op 'wait)
         (mutex-lock m)
         (while (<= count 0)
           (cond-wait c m))
         (set! count (- count 1))
         (mutex-unlock m))
        ((eq? op 'signal)
         (mutex-lock m)
         (set! count (+ count 1))
         (cond-signal c)
         (mutex-unlock m))))))

(puts "Loaded pthreads utilities with safe coordination patterns.\n")
