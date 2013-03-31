#lang racket

;; m-quad.rkt
;; a mode that draws a rotating quad

(require "../lib/mode.rkt"
         "../lib/gl-geometry.rkt"
         "../lib/gl-letters.rkt"
         sgl)

;; ============================================================
(define time 0)
(define color '(1 0 1))

(define (draw)
  (gl-push-matrix)
  ;; (gl-rotate (* time 45)
  ;;            0 0 1)
  (apply gl-color color)
  (gl-quad)
  (gl-color .8 .8 .8 1)
  (gl-scale .2
            .2
            1)
  (gl-translate 0 0 -1)
  (gl-font "hello world!")
  (gl-pop-matrix))

(define (tick dt)
  (set! time (+ time dt)))

(define (key e)
  (set! color (list (random)
                    (random)
                    (random))))

(register-mode 'quad
               ;;#:on-start 
               ;;#:on-quit  
               #:on-draw  draw
               #:on-tick  tick
               #:on-char  key
               )
