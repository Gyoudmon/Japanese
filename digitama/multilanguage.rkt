#lang racket/base

(provide (all-defined-out))

(require scribble/core)

(require racket/list)
(require racket/string)

(require [except-in pict table])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define chinese
  (lambda [#:font [font 'system] . contents]
    (define (content->string elem)
      (cond [(string? elem) elem]
            [(element? elem) (map content->string (element-content elem))]
            [else (format "~a" elem)]))
    
    (make-traverse-element
     (λ [get set]
       (if (member 'latex (get 'scribble:current-render-mode '(latex)))
           (text (string-join (flatten (map content->string contents)) " ") font 12)
           contents)))))
