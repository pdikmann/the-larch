#lang racket

;; No. 1: The Larch
;; is a 3d viewer
;; that lets you view your generative shit
;; easily extendable by sandboxing visuals into 'modes'

(require "lib/gl-window.rkt"    ; frame
         "lib/gl-timer.rkt"     ; updates & refresh
         "lib/gl-geometry.rkt"  ; GL collection
         ;; ---------------- moes
         "modes/mode.rkt"
         "modes/m-quad.rkt"
         ;; temporary: for drawing
         sgl)

;; ============================================================ model
(define camera
  (new
   (class object%
     (super-new)
     ;; ------------------------ data
     (define rotation (/ pi 2))
     (define velocity 0)
     (define turn-speed (/ pi 60))
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

;; ============================================================ Events
(define (keyboard e)
  (let ([key (send e get-key-code)]
        ;;[release (send e get-key-release-code)]
        )
    (case key
      [(left) (send camera turn 'left)]
      [(right) (send camera turn 'right)]
      [(release) (send camera stop-moving)]
      [else (mode-key e)])))

(define (tick)
  ;; clear
  (gl-clear-color 1 1 1 1)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  ;; camera
  (send camera update)
  (send camera gl-run)
  ;; model
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  ;; base
  (gl-color 0 0 0 1)
  (gl-grid)
  ;; mode
  (mode-tick (send timer get-delta-time))
  (mode-draw)
  )

;; ============================================================ Go!
(send canvas paint-with tick)
(send canvas on-char-with keyboard)
(send timer start 16)
