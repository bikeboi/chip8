;; CPU Processing module
;;
;; Author: bikeboi
;;
;; This contains all the instructions for running the CPU part of the emulator. This includes:
;; - Memory representation
;; - Processor state representation
;; - Opcodes

#lang racket

;; General Helper functions
;;; Partial function definition
(define-syntax defpart
  (syntax-rules ()
    [(defpart (id args ...) body)
     (define id
       (curry (lambda (args ...) body)))]
    [(defpart id expr)
     (define id (curry expr))]))

;; Flip binary function args
(define (flip f)
  (curry
   (λ (x y) (f y x))))

;; Apply function followed by modulus
(define (f-mod mod f)
  (λ (x) (modulo (f x) mod)))

;; Partial vector update
(defpart vec-set
  (curry (lambda (i v vec)
           (begin
             (vector-set! vec i v)
             vec))))

;; Partial vector access
(defpart vec-get
  (curry (lambda (i vec)
           (vector-ref vec i))))


;;; CPU Stuff
;; CPU initialization
(define (init-mem n)
  (make-vector n 0))

;; State representation
(define (init-cpu)
  (hash
   'pc    #x200                           ; Program counter starts at 0x200
   'sp    #xF                             ; Stack pointer grows downwards, so starts at top 0xF
   'i     #x0                             ; Address register
   'st    #x0                             ; Sound timer = 0
   'dt    #x0                             ; Delay timer = 0
   'reg   (init-mem #x10)                 ; Registers
   'mem   (init-mem #x1000)               ; Memory
   'stack (init-mem #x10)))               ; Stack

;; CPU access, modification, and execution
;; cpu-get: key -> (cpu -> cpu . a)
(define (cpu-get key [f identity])
  (λ (cpu)
    (cons cpu
          (f (hash-ref cpu key)))))

;; cpu-modify: key -> (a -> b) -> (cpu -> cpu . b)
(define (cpu-modify key f)
  (λ (cpu)
    (let ([new-cpu (hash-update cpu key f)])
      (cons new-cpu '()))))


;; cpu-run: cpu -> (cpu -> cpu . a) -> (cpu . a)
(define (cpu-run cpu cpu-f)
  (cpu-f cpu))

;; cpu-run-: cpu -> (cpu -> cpu . a) -> a
(define cpu-exec
  (compose cdr cpu-run))

;;; -- Monadic CPU functions --
;; Identity CPU function (does nothing but yield value)
;; cpu-return: a -> (cpu -> cpu . a)
(define (cpu-return a)
  (λ (cpu) (cons cpu a)))

;; CPU Monadic Bind
;; >>=: (cpu -> cpu . a) -> (a -> (cpu -> cpu . b)) -> (cpu -> cpu . b)
(define (>>= f g)
  (λ (cpu)
    (match-let ([(cons new-cpu a) (f cpu)])
      ((g a) new-cpu))))

;; Bind ignoring first result
(define (>> f g)
  (>>= f (const g)))

;; Bind ignoring last result
;; >>: (cpu -> cpu . a) -> (a -> (cpu -> cpu . b)) -> (cpu -> cpu . a)
(define (<< f g)
  (>>= f (λ (a) (>>= g (const (cpu-return a))))))


;; Multivariate monadic functions
(define (>>=* m . ms) (foldl (flip >>=) m ms))
(define (>>* m . ms) (foldl (flip >>) m ms))
(define (<<* m . ms) (foldl (flip <<) m ms))

;;; -- Status access and modification --
(define read-pc (cpu-get 'pc))
(define read-sp (cpu-get 'sp))
(define read-st (cpu-get 'st))
(define read-dt (cpu-get 'dt))
(define read-i  (cpu-get 'i))

(define (write-pc v) (cpu-modify 'pc (const v)))
(define (write-sp v) (cpu-modify 'sp (const v)))
(define (write-st v) (cpu-modify 'st (const v)))
(define (write-dt v) (cpu-modify 'dt (const v)))
(define (write-i v)  (cpu-modify 'i (const v)))

(define inc-pc (cpu-modify 'pc (f-mod #x1000 add1)))
(define dec-pc (cpu-modify 'pc (f-mod #x1000 sub1)))
(define inc-sp (cpu-modify 'sp (f-mod #x10 add1)))
(define dec-sp (cpu-modify 'sp (f-mod #x10 sub1)))

;;;  -- Memory access and modification --
;; Helpers

;; Generalized memory access and modification
(defpart (memblock-read key)
  (lambda (addr)
    (cpu-get key
              (vec-get addr))))

(defpart (memblock-write key)
  (lambda (addr val)
    (cpu-modify key
              (vec-set addr val))))

;; Memory
(define mem-read (memblock-read 'mem))
(define mem-write (memblock-write 'mem))

;; Registers
(define reg-read (memblock-read 'reg))
(define reg-write (memblock-write 'reg))

;; Stack
(define stack-read
  (>>= (cpu-get 'sp)
       (memblock-read 'stack)))

(define (stack-write v)
  (>>= (cpu-get 'sp)
       ((flip (memblock-write 'stack)) v)))

;; Specialized stacks functions (these are a little more specialized)
(define pop-stack (>> inc-sp stack-read))
(define (push-stack v) (<< (stack-write v) dec-sp))
