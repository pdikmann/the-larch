#lang racket

;; m-quad.rkt
;; a mode that draws a rotating quad

(require "../lib/mode.rkt"
         "../lib/gl-geometry.rkt"
         "../lib/gl-extend.rkt"
         "../lib/gl-letters.rkt"
         (only-in "../lib/utils.rkt"
                  x y z)
         sgl
         (only-in plot/utils
                  v+ v*))

;; ============================================================ Model
(struct part (position direction age)
        #:mutable)

;; ============================================================ Data
(define spawn-timer 0)
;;(define ticles '())
(define ticles-hash (make-hash))

;; ============================================================ Helpers
(define (spawn-part)
  (hash-set! ticles-hash
             (gensym)
             (part (vector (- (random) .5) ; pos
                           0
                           (- (random) .5))
                   (vector 0 ; dir
                           (- (+ .3 (random)))
                           0)
                   0))
  ;; (set! ticles (cons (part (vector 0 0 0)
  ;;                          (vector 0 -1 0)
  ;;                          0)
  ;;                    ticles))
  )

(define (update-part dt n p)
  (set-part-position! p (v+ (part-position p)
                            (v* (part-direction p)
                                dt)))
  (set-part-age! p (+ (part-age p)
                      dt))
  (when (> (part-age p) 3)
    (hash-remove! ticles-hash n)))

;; ============================================================ Main
(define (start)
  (set! spawn-timer 0)
  ;;(set! ticles '())
  (set! ticles-hash (make-hash)))

(define (draw)
  ;; quad at each particle
  (for ([p (hash-values ticles-hash)])
    (with-gl-matrix
     (gl-translate (x (part-position p))
                   (y (part-position p))
                   (z (part-position p)))
     (gl-scale .1 .1 .1)
     (gl-quad))))

(define (tick dt)
  ;; spawning
  (set! spawn-timer (+ spawn-timer dt))
  (when (> spawn-timer .2)
    (spawn-part)
    (set! spawn-timer (- spawn-timer .2)))
  ;; handling
  (hash-for-each ticles-hash (curry update-part dt)))

(define (key e) #t)

(register-mode 'particles
               #:on-start start
               ;;#:on-quit  
               #:on-draw  draw
               #:on-tick  tick
               #:on-char  key
               )
