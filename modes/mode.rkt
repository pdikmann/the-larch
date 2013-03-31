#lang racket

;; mode.rkt
;; supply a central register and hooks for modes

(provide register-mode
         invoke-mode
         get-modes
         (rename-out [draw mode-draw]
                     [tick mode-tick]
                     [keyboard mode-key]))

;; ============================================================
(struct mode (start quit draw tick char)) ; mode hooks

(define register (make-hash))
(define current 'default)

;; ============================================================
(define (current-mode)
  (hash-ref register current))

;; ------------------------ public
(define (register-mode name
                       #:on-start [start (lambda () #t)]
                       #:on-quit  [quit (lambda () #t)]
                       #:on-draw  [draw (lambda () #t)]
                       #:on-tick  [tick (lambda (dt) #t)]
                       #:on-char  [char (lambda (e) #t)])
  (hash-set! register name (mode start
                             quit
                             draw
                             tick
                             char)))

(define (get-modes)
  (hash-keys register))

(define (invoke-mode name)
  ((mode-quit (current-mode)))
  (set! current name)
  ((mode-start (current-mode))))

(define (draw)
  ((mode-draw (current-mode))))

(define (tick dt)
  ((mode-tick (current-mode)) dt))

(define (keyboard e)
  ((mode-char (current-mode)) e))

;; ============================================================ Go!
(register-mode 'default)

