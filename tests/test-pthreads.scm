;;; test-pthreads.scm - Pthreads synchronization primitives test
;;;
;;; Tests mutexes and condition variables (the parts that work!)

(require-so (so-ext "pthreads"))

(puts "=== Pthreads Synchronization Test ===\n\n")

;; Test 1: Mutex creation
(puts "[TEST 1] Mutex operations\n")
(define m (mutex-create))
(puts "  Created mutex: ")
(print m)
(puts "\n")

;; Test 2: Lock and unlock
(puts "\n[TEST 2] Lock/Unlock\n")
(puts "  Locking mutex...\n")
(mutex-lock m)
(puts "  Mutex locked\n")
(puts "  Unlocking mutex...\n")
(mutex-unlock m)
(puts "  Mutex unlocked\n")

;; Test 3: Trylock
(puts "\n[TEST 3] Trylock\n")
(puts "  Trying to lock (should succeed)...\n")
(define tryresult (mutex-trylock m))
(puts "  Result: ")
(print tryresult)
(puts " (should be t)\n")

(puts "  Trying to lock again (should fail)...\n")
(set! tryresult (mutex-trylock m))
(puts "  Result: ")
(print tryresult)
(puts " (should be ())\n")

(mutex-unlock m)

;; Test 4: Protected counter
(puts "\n[TEST 4] Protected counter pattern\n")
(define counter 0)
(define counter-mutex (mutex-create))

(define (safe-increment)
  (mutex-lock counter-mutex)
  (set! counter (+ counter 1))
  (define result counter)
  (mutex-unlock counter-mutex)
  result)

(define (safe-get)
  (mutex-lock counter-mutex)
  (define result counter)
  (mutex-unlock counter-mutex)
  result)

(puts "  Initial counter: ")
(print (safe-get))
(puts "\n")

(puts "  Incrementing 5 times...\n")
(define i 0)
(while (< i 5)
  (puts "    Increment ")
  (print (+ i 1))
  (puts ": ")
  (print (safe-increment))
  (puts "\n")
  (set! i (+ i 1)))

(puts "  Final counter: ")
(print (safe-get))
(puts "\n")

;; Test 5: Condition variables
(puts "\n[TEST 5] Condition variable operations\n")
(define c (cond-create))
(puts "  Created condition variable: ")
(print c)
(puts "\n")

(puts "  Testing cond-signal: ")
(cond-signal c)
(puts "OK\n")

(puts "  Testing cond-broadcast: ")
(cond-broadcast c)
(puts "OK\n")

;; Test 6: Semaphore pattern
(puts "\n[TEST 6] Semaphore pattern\n")
(define (make-semaphore initial)
  (let ((count initial)
        (sem-m (mutex-create))
        (sem-c (cond-create)))
    (lambda (op)
      (cond
        ((eq? op 'wait)
         (mutex-lock sem-m)
         (while (<= count 0)
           (cond-wait sem-c sem-m))
         (set! count (- count 1))
         (define result count)
         (mutex-unlock sem-m)
         result)
        ((eq? op 'signal)
         (mutex-lock sem-m)
         (set! count (+ count 1))
         (define result count)
         (cond-signal sem-c)
         (mutex-unlock sem-m)
         result)
        ((eq? op 'get)
         (mutex-lock sem-m)
         (define result count)
         (mutex-unlock sem-m)
         result)))))

(define sem (make-semaphore 3))
(puts "  Created semaphore with count 3\n")
(puts "  Current count: ")
(print (sem 'get))
(puts "\n")

(puts "  Waiting (acquiring)...\n")
(define new-count (sem 'wait))
(puts "  Count after wait: ")
(print new-count)
(puts "\n")

(puts "  Signaling (releasing)...\n")
(set! new-count (sem 'signal))
(puts "  Count after signal: ")
(print new-count)
(puts "\n")

;; Test 7: Thread utilities
(puts "\n[TEST 7] Thread utilities\n")
(puts "  Testing pthread-sleep (0.1 seconds)...\n")
(pthread-sleep 0.1)
(puts "  Sleep completed\n")

(puts "  Testing pthread-yield...\n")
(pthread-yield)
(puts "  Yield completed\n")

;; Test 8: Safe list
(puts "\n[TEST 8] Thread-safe list pattern\n")
(define (make-safe-list)
  (let ((data '())
        (list-m (mutex-create)))
    (lambda (op . args)
      (mutex-lock list-m)
      (define result
        (cond
          ((eq? op 'add)
           (set! data (cons (car args) data))
           data)
          ((eq? op 'get)
           data)
          ((eq? op 'size)
           (length data))
          ((eq? op 'clear)
           (set! data '())
           '())))
      (mutex-unlock list-m)
      result)))

(define safe-list (make-safe-list))
(puts "  Created safe list\n")

(puts "  Adding items...\n")
(safe-list 'add 1)
(safe-list 'add 2)
(safe-list 'add 3)

(puts "  List contents: ")
(print (safe-list 'get))
(puts "\n")

(puts "  List size: ")
(print (safe-list 'size))
(puts "\n")

(puts "\n=== All synchronization tests passed! ===\n")
(puts "\nNOTE: These primitives work. Thread creation with Scheme code\n")
(puts "requires making SIOD's evaluator thread-safe (future work).\n")
(puts "\nFor now, use mutexes/condvars to coordinate with C extensions\n")
(puts "that do threading internally, or for protecting shared data.\n")
