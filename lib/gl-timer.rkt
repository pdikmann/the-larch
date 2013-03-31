#lang racket

;; a timer used to refresh the gl canvas.
;; is intentionally coupled to windows.rkt for use of auth-refresh.

(provide timer)

;; ------------------------------------------------------------
;; implementation

(require "gl-window.rkt"
         racket/gui)

(define frame-timer%
  (class timer%
    (super-new)
    ;; ------------------------ private
    (define frames 0)
    (define last-time 0)
    (define delta-time 0)
    (define notify-fn (lambda () (send canvas auth-refresh)))
    ;; ------------------------ overrides
    (define/override (notify)
      (let ([this-time (current-inexact-milliseconds)])
        (set! delta-time (/ (- this-time
                               last-time)
                            1000))
        (set! last-time this-time))
      (notify-fn)
      (set! frames (+ 1 frames)))
    ;; ------------------------ public
    (define/public (get-frames) frames)
    (define/public (get-delta-time) delta-time)
    ;; set new event callback
    (define/public (notify-with fn)
      (set! notify-fn fn))))

(define timer (new frame-timer%
                   [interval #f]))
