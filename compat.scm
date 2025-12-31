;;; SIOD Compatibility Layer
;;; Works with minimal SIOD implementations
;;; Load this FIRST before any other libraries

;;; ============================================
;;; Core Compatibility: defined? function
;;; ============================================


(define modulo %)

;;; SIOD doesn't have defined? built-in, so we define it first
(define (defined? sym)
  "Check if a symbol is defined"
  (symbol-bound? sym (the-environment)))

;;; ============================================
;;; I/O Compatibility - Try multiple options
;;; ============================================

;;; Try to define display - check what exists
(if (not (defined? 'display))
    (cond 
      ((defined? 'princ)
       (define (display x) (princ x)))
      ((defined? 'prin1)
       (define (display x) (prin1 x)))
      ((defined? 'print)
       (define (display x) (print x)))
      (else
       (define (display x) x))))  ; Last resort - do nothing

;;; Try to define newline
(if (not (defined? 'newline))
    (cond
      ((defined? 'terpri)
       (define (newline) (terpri)))
      ((defined? 'princ)
       (define (newline) (princ "\n")))
      ((defined? 'prin1)
       (define (newline) (prin1 "\n")))
      (else
       (define (newline) #t))))  ; Last resort - do nothing

;;; ============================================
;;; Other Common Missing Functions
;;; ============================================

;;; string->symbol (might be called intern or oblist)
(if (not (defined? 'string->symbol))
    (if (defined? 'intern)
        (define string->symbol intern)
        (define (string->symbol s) s)))

;;; symbol->string (might be called symbol-name)
(if (not (defined? 'symbol->string))
    (if (defined? 'symbol-name)
        (define symbol->string symbol-name)
        (define (symbol->string s) s)))

;;; apply (might already exist)
(if (not (defined? 'apply))
    (define (apply f args)
      (eval (cons f args))))

;;; modulo is % 
(if (not (defined? 'modulo))
    (define modulo %))

;;; ============================================
;;; Predicates that might be missing
;;; ============================================

(if (not (defined? 'null?))
    (define (null? x) (eq? x '())))

(if (not (defined? 'pair?))
    (define (pair? x) (not (or (null? x) (atom? x)))))

(if (not (defined? 'list?))
    (define (list? x)
      (or (null? x) (pair? x))))

(if (not (defined? 'number?))
    (define (number? x)
      (or (integer? x) (flonum? x))))

(if (not (defined? 'symbol?))
    (define (symbol? x)
      ;; In minimal SIOD, symbols are atoms that aren't numbers
      (and (atom? x) (not (number? x)))))

;;; ============================================
;;; Basic List Functions
;;; ============================================

(if (not (defined? 'caar))
    (define (caar x) (car (car x))))

(if (not (defined? 'cadr))
    (define (cadr x) (car (cdr x))))

(if (not (defined? 'cdar))
    (define (cdar x) (cdr (car x))))

(if (not (defined? 'cddr))
    (define (cddr x) (cdr (cdr x))))

;;; ============================================
;;; Boolean compatibility
;;; ============================================

;;; Some SIODs use #t/#f, some use t/nil, some use ()
(if (not (defined? '#t))
    (define #t 't))

(if (not (defined? '#f))
    (define #f '()))

;;; ============================================
;;; Message
;;; ============================================

(cond
  ((defined? 'princ)
   (princ "SIOD Compatibility Layer loaded\n"))
  ((defined? 'prin1)
   (prin1 "SIOD Compatibility Layer loaded")
   (prin1 "\n"))
  ((defined? 'print)
   (print "SIOD Compatibility Layer loaded"))
  (else #t))
