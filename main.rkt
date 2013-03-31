#lang racket

(require "lib/gl-window.rkt"
         "lib/gl-timer.rkt"
         "lib/gl-geometry.rkt"
         ;; temporary: for drawing
         sgl
         )



(define (draw)
  (let ([c (/ (modulo (send timer get-frames)
                      256)
              255)])
    ;;(gl-clear-color 1 1 1 1)
    (gl-clear-color c c c 1)
    )
  ;;
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-perspective 90
                  (/ (send canvas get-width)
                     (send canvas get-height))
                  .1 10)
  ;;(gl-translate 0 0 -10)
  (let ([angle (* (/ (modulo (send timer get-frames)
                             256)
                     255)
                  pi)])
    (gl-look-at (* (cos angle) 5)
                1
                (* (sin angle) 5)
                0 0 0
                0 1 0))
  ;;
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-color 1 0 1 1)
  (gl-quad))

(send canvas paint-with draw)
(send timer start 16)
