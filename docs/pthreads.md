# Pthreads Module for SIOD

## Overview

The `pthreads` module provides POSIX threads synchronization primitives (mutexes and condition variables) for SIOD. 

**IMPORTANT LIMITATION**: SIOD's Scheme evaluator is **not thread-safe**. You cannot safely run Scheme code in separate threads using `pthread-create`. However, the synchronization primitives (mutexes and condition variables) are fully functional and useful for:

1. Coordinating with C extensions that use threads internally
2. Protecting shared data structures
3. Building thread-safe utilities
4. Eventually: coordinating worker threads for numerical computation

## What Works

✅ **Mutexes** - Lock/unlock for protecting shared data  
✅ **Condition Variables** - Wait/signal for thread coordination  
✅ **Thread utilities** - Sleep, yield  

## What Doesn't Work (Yet)

❌ **pthread-create with Scheme functions** - SIOD's evaluator has global state  
❌ **Arbitrary Scheme evaluation in threads** - Would require major refactoring  

## Future Direction

To enable true parallel computation in SIOD, we have options:

1. **Task Queue Pattern**: C-level worker threads that process numerical tasks
2. **Thread-Safe Evaluator**: Refactor SIOD's evaluator for thread safety  
3. **External Workers**: Spawn separate SIOD processes and coordinate via IPC  

For mathematical simulations, option #1 (task queues) is most practical.

## Loading the Module

```scheme
(require-so (so-ext "pthreads"))
```

## Mutex Operations

### Creating Mutexes

#### `(mutex-create)`
Creates a new mutex.

```scheme
(define m (mutex-create))
```

**Returns:** Mutex handle

### Locking and Unlocking

#### `(mutex-lock mutex)`
Locks the mutex. Blocks if already locked.

```scheme
(mutex-lock m)
;; critical section
(mutex-unlock m)
```

#### `(mutex-unlock mutex)`
Unlocks the mutex.

#### `(mutex-trylock mutex)`
Attempts to lock the mutex without blocking.

```scheme
(if (mutex-trylock m)
    (begin
      ;; Got the lock
      (do-something)
      (mutex-unlock m))
    (puts "Mutex was busy\n"))
```

**Returns:** `t` if lock acquired, `()` if busy

## Condition Variable Operations

### Creating Condition Variables

#### `(cond-create)`
Creates a new condition variable.

```scheme
(define c (cond-create))
```

**Returns:** Condition variable handle

### Wait and Signal

#### `(cond-wait cond mutex)`
Waits on the condition variable. Must be called with mutex locked.

```scheme
(mutex-lock m)
(while (not ready?)
  (cond-wait c m))
;; condition is now true
(mutex-unlock m)
```

**Important:** The mutex is atomically unlocked while waiting and re-locked when woken.

#### `(cond-signal cond)`
Wakes up one thread waiting on the condition variable.

```scheme
(mutex-lock m)
(set! ready? #t)
(cond-signal c)
(mutex-unlock m)
```

#### `(cond-broadcast cond)`
Wakes up all threads waiting on the condition variable.

```scheme
(cond-broadcast c)  ; Wake everyone up
```

## Thread Utilities

### `(pthread-sleep seconds)`
Sleep for specified seconds (can be fractional).

```scheme
(pthread-sleep 0.5)  ; Sleep 500ms
(pthread-sleep 2)    ; Sleep 2 seconds
```

### `(pthread-yield)`
Yield the CPU to other threads.

```scheme
(pthread-yield)
```

## Practical Patterns

### Pattern 1: Protected Counter

```scheme
(define counter 0)
(define counter-mutex (mutex-create))

(define (increment-counter)
  (mutex-lock counter-mutex)
  (set! counter (+ counter 1))
  (define result counter)
  (mutex-unlock counter-mutex)
  result)

(define (get-counter)
  (mutex-lock counter-mutex)
  (define result counter)
  (mutex-unlock counter-mutex)
  result)
```

### Pattern 2: Producer-Consumer with Condition Variables

```scheme
(define queue '())
(define queue-mutex (mutex-create))
(define queue-cond (cond-create))

(define (enqueue item)
  (mutex-lock queue-mutex)
  (set! queue (append queue (list item)))
  (cond-signal queue-cond)  ; Wake up consumer
  (mutex-unlock queue-mutex))

(define (dequeue)
  (mutex-lock queue-mutex)
  (while (null? queue)
    (cond-wait queue-cond queue-mutex))
  (define item (car queue))
  (set! queue (cdr queue))
  (mutex-unlock queue-mutex)
  item)
```

### Pattern 3: Semaphore

```scheme
(define (make-semaphore initial-count)
  (let ((count initial-count)
        (m (mutex-create))
        (c (cond-create)))
    (lambda (operation)
      (cond
        ((eq? operation 'wait)
         (mutex-lock m)
         (while (<= count 0)
           (cond-wait c m))
         (set! count (- count 1))
         (mutex-unlock m))
        ((eq? operation 'signal)
         (mutex-lock m)
         (set! count (+ count 1))
         (cond-signal c)
         (mutex-unlock m))))))

;; Usage
(define sem (make-semaphore 3))
(sem 'wait)    ; Acquire
(sem 'signal)  ; Release
```

## Use Cases

### 1. Thread-Safe Data Structures

Perfect for protecting shared data:

```scheme
(define (make-safe-list)
  (let ((data '())
        (m (mutex-create)))
    (lambda (op . args)
      (mutex-lock m)
      (define result
        (cond
          ((eq? op 'add) 
           (set! data (cons (car args) data))
           data)
          ((eq? op 'get)
           data)
          ((eq? op 'clear)
           (set! data '())
           '())))
      (mutex-unlock m)
      result)))
```

### 2. Coordinating with C Extensions

If you write C extensions that use threads internally, use mutexes to protect shared state:

```c
// In your C extension
static pthread_mutex_t worker_mutex = PTHREAD_MUTEX_INITIALIZER;
static int worker_result = 0;

void* worker_thread(void* arg) {
    // Do computation
    int result = expensive_computation();
    
    pthread_mutex_lock(&worker_mutex);
    worker_result = result;
    pthread_mutex_unlock(&worker_mutex);
    
    return NULL;
}
```

Then coordinate from Scheme using the mutex primitives.

## Performance Notes

- Mutex operations are fast (nanoseconds when uncontended)
- Condition variable wait/signal is efficient
- No spinning - threads properly sleep when waiting

## Thread Safety Rules

1. **Always lock before accessing shared data**
2. **Hold locks for minimal time**
3. **Avoid deadlocks** - always acquire locks in the same order
4. **Use condition variables** instead of polling
5. **Don't nest locks** unless absolutely necessary

## Debugging Tips

If you see deadlocks:
- Check lock acquisition order
- Ensure all code paths unlock
- Use trylock for diagnostics

## Future Work

To enable full parallel Scheme evaluation:

1. Make garbage collector thread-safe
2. Add thread-local evaluation contexts  
3. Protect global symbol table
4. Add memory barriers for shared data
5. Create thread-safe primitive operations

Or alternatively:
- Build task queue system in C
- Create numerical worker threads
- Expose via simple Scheme API

## See Also

- POSIX threads documentation
- `pthreads-utilities.scm` - High-level patterns
- Future: `parallel.scm` - Parallel computation utilities

## Credits

Part of SIOD-TR (The Reawakening)  
Author: Scáth, 2025  
License: GPL-3.0
