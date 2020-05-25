#lang racket

(provide (all-defined-out))

(require scribble/core)
(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy]
    (make-traverse-element
     (λ [get set]
       (if (member 'latex (get 'scribble:current-render-mode '(latex)))
           (make-multiarg-element "ruby" (list base ruy))
           (elem base))))))

(define define-term
  (lambda [en kenji hiragana #:key [key #false]]
    (list (deftech en #:key key)
          ~
          "「" (ruby kenji hiragana) "」")))
