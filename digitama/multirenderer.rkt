#lang racket/base

(provide (all-defined-out))

(require scribble/base)
(require scribble/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex
  (lambda []
    (make-traverse-element
     (λ [get set]
       (if (member 'latex (get 'scribble:current-render-mode '(latex)))
           (elem "LaTex")
           (elem "LaTex"))))))
