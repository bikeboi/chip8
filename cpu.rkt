;; CPU Processing module
;;
;; Author: bikeboi
;;
;; This contains all the instructions for running the CPU part of the emulator. This includes:
;; - Memory representation
;; - Processor state representation
;; - Opcodes

#lang racket

;; State representation
(struct cpu (pc sp st dt regs mem stack))

;; CPU initialization
(define init-cpu
  (cpu #x200                              ; Program counter starts at 0x200
       #xF                                ; Stack pointer grows downwards, so starts at top 0xF
       #x0                                ; Sound timer = 0
       #x0                                ; Delay timer = 0
       (make-vector 16 #x0)               ; Registers empty
       (make-vector 4096 #x0)             ; Memory empty
       (make-vector 16 #x0)))             ; Stack empty

;;; Elementary Memory Operations
