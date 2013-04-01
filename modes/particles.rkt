#lang racket

;; particles.rkt
;; a mode that plots particles

(require "../lib/mode.rkt"
         "../lib/gl-geometry.rkt"
         "../lib/gl-extend.rkt"
         "../lib/gl-letters.rkt"
         sgl
         (only-in plot/utils
                  v+
                  v-
                  v*
                  vmag)
         (only-in "../lib/utils.rkt"
                  x y z))

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
             (part (vector (* 5 (- (random) .5)) ; pos
                           0
                           (- (random) .5))
                   (vector 0 ; dir
                           -.8 ;(- (+ .3 (random)))
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
  (when (> (part-age p) 8)
    (hash-remove! ticles-hash n)))

(define (close-triplet p)
  (with-handlers ([exn:fail? (lambda (x) '())]) ; sometimes there are less than 3 particles
    (map (lambda (pair)
           (hash-ref ticles-hash (car pair)))
         (take (sort (hash-map ticles-hash
                               (lambda (k v)
                                 (cons k (vmag (v- (part-position v)
                                                   (part-position p))))))
                     < #:key cdr)
               3))))

(define (next-up-triplet p)
  (with-handlers ([exn:fail? (lambda (x) '())]) ; sometimes there are less than 3 particles
    (map (lambda (pair)
           (hash-ref ticles-hash (car pair)))
         (take (filter (lambda (pair) (negative? (cdr pair)))
                       (sort (hash-map ticles-hash
                                       (lambda (k v)
                                         (cons k (y (v- (part-position v)
                                                        (part-position p))))))
                             > #:key cdr))
               3))))

;; ============================================================ Main
(define (start)
  (set! spawn-timer 0)
  ;;(set! ticles '())
  (set! ticles-hash (make-hash)))

(define (draw)
  (for ([p (hash-values ticles-hash)])
    ;; quad at each particle
    (gl-color 0 0 0 1)
    (with-gl-matrix
     (gl-translate (x (part-position p))
                   (y (part-position p))
                   (z (part-position p)))
     (gl-scale .02 .02 .02)
     (gl-quad))
    ;; triangle for each triples
    (gl-color 1 0 0 1)
    (gl-begin 'triangles)
    (for (;;[tp (close-triplet p)]
          [tp (next-up-triplet p)]
          )
      (apply gl-vertex (vector->list (part-position tp))))
    (gl-end)))

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
