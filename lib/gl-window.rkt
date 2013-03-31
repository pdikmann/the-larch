#lang racket

;; main window and gl canvas with handy helper functions built-in

(provide canvas
         (struct-out window)
         (rename-out [*config* main-window]))

;; ============================================================ config
(struct window (width height label))
(define *config* (window 512
                         600
                         "No. 1: The Larch"))

;; ============================================================
(require racket/gui
         sgl
         sgl/gl
         sgl/gl-vectors)

;; the window
(define killer-frame%
  (class frame%
    (super-new)
    (define/augment (on-close)
      (exit))))

;; the canvas
(define gl-canvas%
  (class canvas%
    (inherit with-gl-context
             swap-gl-buffers
             get-parent
             refresh)
    (super-new (style '(gl no-autoclear)))
    ;; ------------------------------------------------------------ private
    ;; authorized refresh
    (define auth? #f)   
    (define (auth-grant) (set! auth? #t))
    (define (auth-remove) (set! auth? #f))
    ;;drawing function, replacable
    (define paint-fn
      (lambda ()
        (gl-clear-color 1 1 1 0.0)
        (gl-clear 'color-buffer-bit 'depth-buffer-bit)
        ;;
        (gl-matrix-mode 'modelview)
        (gl-load-identity)))
    ;; events, replacable
    (define on-event-fn (lambda (e) #t))
    (define on-char-fn (lambda (e) #t))
    ;; overrides
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-viewport 0 0 width height)
         ;; projection matrix
         (gl-matrix-mode 'projection)
         (gl-load-identity)
         (gl-perspective 90
                          (/ width height)
                          .1 10)
         ;;(gl-translate 0 0 -1)
         (gl-look-at 0 1 -1
                     0 0 0
                     0 1 0)
         ;; (gl-ortho 0 width
         ;;           height 0
         ;;           0 10)
         (gl-translate 0 0 -1)
         ;; model matrix
         (gl-matrix-mode 'modelview)
         (gl-load-identity)
         ;; enables
         (gl-enable 'depth-test)
         (gl-enable 'blend)
         ;;(gl-enable 'blend)
         (gl-blend-func 'src-alpha 'one-minus-src-alpha)
         ;;(gl-enable 'cull-face 'lighting 'texture-2d 'depth-test)
         ;;(gl-enable 'scissor-test)
         )))

    (define/override (on-paint)
      (when auth?
        (with-gl-context
         (lambda ()
           (paint-fn)
           (swap-gl-buffers)
           (gl-flush)))
        (auth-remove)))

    (define/override (on-event event) (on-event-fn event))
    (define/override (on-char event) (on-char-fn event))
    ;; ------------------------------------------------------------ public
    ;; authorized refresh
    (define/public (auth-refresh)
      (auth-grant)
      (refresh))
    ;; replace the draw-fn that is used in on-paint
    (define/public (paint-with fn)
      (set! paint-fn fn))
    ;; replace the fn called on any mouse event
    (define/public (on-event-with fn)
      (set! on-event-fn fn))
    ;; replace the fn called on any keyboard event
    (define/public (on-char-with fn)
      (set! on-char-fn fn))))

;; ============================================================
;; instantiation
(define canvas #f)
(define frame #f)

(define (window-init [cfg *config*])
  (when (not canvas)
    (set! frame
          (new killer-frame%
               [label (window-label cfg)]
               [width (window-width cfg)]
               [height (window-height cfg)]
               [style '(no-resize-border)]))
    (set! canvas (new gl-canvas%
                      [parent frame]
                      [min-width (window-width cfg)]
                      [min-height (window-height cfg)]))
    (send frame show #t)))

(window-init)
