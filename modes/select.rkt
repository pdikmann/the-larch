#lang racket

;; default.rk
;; one mode to rule them all

(require "../lib/gl-window.rkt" ; for width/height in gl-ortho
         "../lib/mode.rkt"
         "../lib/gl-geometry.rkt"
         "../lib/gl-letters.rkt"
         "../lib/gl-extend.rkt"
         sgl)

;; ============================================================ Data
(define mode-list '())
(define selection 0)

;; ============================================================ Hooks
(define (start)
  (set! mode-list (get-modes)))

(define (draw)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-ortho 0 (window-width main-window)
            (window-height main-window) 0
            0 10)
  ;;
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  (gl-color .4 .8 .1 1)
  (gl-scale 40
            40
            1)
  (gl-translate 0
                0.3
                0)
  (with-gl-matrix
   (gl-translate 0
                 (* 1.3 selection)
                 0)
   (gl-font "ä"))
  (gl-translate 1 0 0)
  (with-gl-matrix
   (for ([m mode-list])
     (gl-font (symbol->string m))
     (gl-translate 0 1.3 0))))

(define (keyboard e)
  (let ([key (send e get-key-code)])
    (case key
      [(up) (set! selection (modulo (+ (- 2 (length mode-list))
                                         selection)
                                      (length mode-list)))]
      [(down) (set! selection (modulo (+ 1 selection)
                                    (length mode-list)))]
      [(#\  #\) (invoke-mode (list-ref mode-list selection))]
      [else (printf "~a\n" key)])))

(register-mode 'select
               #:on-start start
               #:on-draw  draw
               #:on-char  keyboard
               ;;#:on-tick  tick
               ;;#:on-quit  
)
