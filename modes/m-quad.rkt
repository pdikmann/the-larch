#lang racket

;; m-quad.rkt: a mode that draws a quad

(require "mode.rkt"
         "../lib/gl-geometry.rkt"
         sgl)

(define time 0)

(define (draw)
  (gl-push-matrix)
  (gl-rotate (* time 45)
             0 0 1)
  (gl-color 1 0 1 1)
  (gl-quad)
  (gl-pop-matrix))

(define (tick dt)
  (set! time (+ time dt)))

(register-mode 'quad
               ;;#:on-start 
               ;;#:on-quit  
               #:on-draw  draw
               #:on-tick  tick
               ;;#:on-char  
               )
