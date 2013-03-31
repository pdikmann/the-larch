#lang racket

(require "lib/gl-window.rkt"
         "lib/gl-timer.rkt"
         "lib/gl-geometry.rkt"
         ;; temporary: for drawing
         sgl)

(define camera
  (new
   (class object%
     (super-new)
     ;; ------------------------ data
     (define rotation (/ pi 2))
     (define velocity 0)
     (define turn-speed (/ pi 60))
     ;; ------------------------ private
     ;; ------------------------ public
     (define/public (update)
       (set! rotation (+ rotation
                         velocity)))
     (define/public (stop-moving)
       (set! velocity 0))
     (define/public (turn dir)
       (case dir
         [(left) (set! velocity (- turn-speed))]
         [(right) (set! velocity turn-speed)]))
     (define/public (gl-run)
       (gl-matrix-mode 'projection)
       (gl-load-identity)
       (gl-perspective 90
                       (/ (send canvas get-width)
                          (send canvas get-height))
                       .1 10)
       (gl-look-at (* (cos rotation) 5)
                   2
                   (* (sin rotation) 5)
                   0 0 0
                   0 1 0)))))

(define (keyboard e)
  (let ([key (send e get-key-code)]
        ;;[release (send e get-key-release-code)]
        )
    (case key
      [(left) (send camera turn 'left)]
      [(right) (send camera turn 'right)]
      [(release) (send camera stop-moving)])))

(define (draw)
  ;; (let ([c (/ (modulo (send timer get-frames)
  ;;                     256)
  ;;             255)])
  ;;   (gl-clear-color c c c 1))
  (gl-clear-color 1 1 1 1)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  ;; camera
  (send camera update)
  (send camera gl-run)
  ;; model
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  ;;
  (gl-color 1 0 1 1)
  (gl-quad)
  (gl-color 0 0 0 1)
  (gl-grid))

(send canvas paint-with draw)
(send canvas on-char-with keyboard)
(send timer start 16)
