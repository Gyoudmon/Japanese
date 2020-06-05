#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (ja-thing stx)
  (syntax-case stx []
    [(_ ja-struct ja-type pre-flow ...)
     #'(defthing ja-struct ja-type
         pre-flow ...)]))

(define-syntax (ja-things stx)
  (syntax-case stx []
    [(_ [[ja-struct ja-type] ...] pre-flow ...)
     #'(deftogether
         [(ja-thing ja-struct ja-type) ...]
         pre-flow ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy #:options [options #false]]
    (make-traverse-element
     (λ [get set]
       (cond [(not (handbook-latex-renderer? get)) (elem base)]
             [(list? options) (make-multiarg-element (make-style "ruby" (list (make-command-optional (map ~a options)))) (list base ruy))]
             [else (make-multiarg-element "ruby" (list base ruy))])))))

(define chinese
  (lambda [#:font [font "FandolSong"] . contents]
    (make-traverse-element
     (λ [get set]
       (if (handbook-latex-renderer? get)
           (make-multiarg-element "Chinese" (list font contents))
           contents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-title
  (lambda [en kenji hiragana]
    (list en ~ "|" ~
          (ruby kenji hiragana))))

(define ja-deftech
  (lambda [en kenji hiragana #:key [key #false]]
    (list (deftech en #:key key) ~
          "「" (ruby kenji hiragana) "」")))

(define ja-tech
  (lambda [en kenji hiragana #:key [key #false]]
    (list (tech #:key (or key en)
                en ~
                "「" (ruby kenji hiragana) "」"))))
