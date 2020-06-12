#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (ja-title stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))
              (~optional (~seq #:ja-term? term?) #:defaults ([term? #'#true])))
        ... en0 kenji0 hiragana0 (~optional zh #:defaults ([zh #'#false])))
     #'(let-values ([(en kenji hiragana) (ja-inputs 'en0 'kenji0 'hiragana0)])   
         (make-traverse-element
          (λ [get set!]
            (unless (not term?)
              (ja-terminology get set! key en kenji 'zh (handbook-latex-renderer? get)))
            (list en ~ "|" ~ (ruby kenji hiragana)))))]))

(define-syntax (ja-deftech stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))
              (~optional (~seq #:ja-term? term?) #:defaults ([term? #'#true])))
        ... en0 kenji0 hiragana0 (~optional zh #:defaults ([zh #'#false])))
     #'(let-values ([(en kenji hiragana) (ja-inputs 'en0 'kenji0 'hiragana0)])    
         (make-traverse-element
          (λ [get set!]
            (unless (not term?)
              (ja-terminology get set! key en kenji 'zh (handbook-latex-renderer? get)))
            (list (deftech en #:key key) ~ "「" (ruby kenji hiragana) "」"))))]))

(define-syntax (ja-tech stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false])))
        ... en0 kenji0 hiragana0)
     #'(let-values ([(en kenji hiragana) (ja-inputs 'en0 'kenji0 'hiragana0)])    
         (list (tech #:key (or key en)
                     en ~
                     "「" (ruby kenji hiragana) "」")))]))

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

(define-syntax (ja-example-parse stx)
  (syntax-case stx []
    [(_ [[ja ...] [ruy ...] [en ...]])
     #'(let ([ruby.styles (list (ja-example->ruby-token 'ruy) ...)])
         (list (list (ruby (list (ja-input 'ja) ...) (map car ruby.styles) #:style (map cdr ruby.styles)))
               ;;; NOTE:
               ;; Rubies that displayed under kenjis do not contribute to the height of whole,
               ;;  an empty line is required to avoid overlapping.
               (list "")
               (list (exec (string-join (map symbol->string '(|| en ...)))))))]))

(define-syntax (ja-example stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [[ja ...] [ruy ...] [en ...]] ...)
     #'(make-tamer-indexed-traverse-block
        (λ [type chapter-index current-index]
          (define extag (symbol->string (or 'maybe-tag (gensym 'ex))))
          (define empty-row (list (list "")))
          (define example (format "Example ~a.~a" chapter-index current-index))

          (define example-rows
            (for/list ([example-row (in-list (list (ja-example-parse [[ja ...] [ruy ...] [en ...]]) ...))]
                       [sub (in-naturals 0)])
              (define subexample (string (integer->char (+ 97 sub))))
              (list (tabular #:sep (hspace 1)
                             #:style 'block
                             #:column-properties '(left center left)
                             (list (list ""
                                         (elemtag (string-append extag "." subexample) (envvar subexample))
                                         (tabular #:style 'block example-row)))))))
          
          (values (string->symbol extag)
                  (tabular #:style 'block
                           #:column-properties '(left)
                           #:row-properties (append (list '(())) (make-list (add1 (length example-rows)) (list 'bottom-border)) (list '(())))
                           (append empty-row
                                   (list (list (elemtag extag (envvar example))))
                                   example-rows
                                   empty-row))))
        'ex)]
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [ja ...] [ruy ...] [en ...])
     #'(make-tamer-indexed-traverse-block
        (λ [type chapter-index current-index]
          (define extag (or 'maybe-tag (gensym 'ex)))
          (define empty-row (list (list "")))
          (define example (format "Example ~a.~a" chapter-index current-index))
          (define example-row (ja-example-parse [[ja ...] [ruy ...] [en ...]]))
          
          (values extag
                  (tabular #:style 'block
                           #:column-properties '(left)
                           #:row-properties '(() bottom-border () () 'bottom-border ())
                           (append empty-row
                                   (list (list (elemtag (symbol->string extag) (tt example))))
                                   example-row
                                   empty-row))))
        'ex)]))

(define-syntax (ja-exref stx)
  (syntax-parse stx #:datum-literals []
    [(_ extag:id)
     #'(make-tamer-indexed-elemref
        (λ [type chapter-index maybe-index]
          (elemref (symbol->string 'extag)
                   (subscript (format "~a~a.~a" type chapter-index (or maybe-index '?)))))
        'ex 'extag)]
    [(_ extag:id subtag:id)
     #'(make-tamer-indexed-elemref
        (λ [type chapter-index maybe-index]
          (elemref (format "~a.~a" 'extag 'subtag)
                   (subscript (format "~a~a.~a~a" type chapter-index (or maybe-index '?) 'subtag))))
        'ex 'extag)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy #:options [options #false] #:style [styles null]]
    (define bases (ja-ruby-content base))
    (define rubies (ja-ruby-content ruy))
    (define rsize (length rubies))
    (define-values (ssize slast)
      (cond [(null? styles) (values 0 "ruby")]
            [(list? styles) (values (length styles) (last styles))]
            [else (values 0 styles)]))
    
    (for/list ([b (in-list bases)]
               [i (in-naturals 0)])
      (define r (and (< i rsize) (list-ref rubies i)))
      (define s0 (if (< i ssize) (list-ref styles i) slast))
      (define s (cond [(not (eq? s0 'auto)) s0]
                      [else (let ([eidx (sub1 (string-length b))])
                              (cond [(= eidx 0) "exmprubyvc"] ; 'auto is designed for examples 
                                    [else (let* ([sgana? (ja-hiragana? (string-ref b 0))]
                                                 [egana? (ja-hiragana? (string-ref b eidx))]
                                                 [egana? (or (and egana?)
                                                             (and (ja-kigou? (string-ref b eidx))
                                                                  (ja-hiragana? (string-ref b (sub1 eidx)))))])
                                            (cond [(eq? sgana? egana?) "exmprubyvc"]
                                                  [(not sgana?) "exmprubyvl"]
                                                  [else "exmprubyvr"]))]))]))
      
      (make-traverse-element
       (λ [get set]
         (cond [(not (handbook-latex-renderer? get)) b]
               [(or (not r) (equal? r "")) b]
               [(pair? options) (make-multiarg-element (make-style s (list (make-command-optional (map ~a options)))) (list b r))]
               [else (make-multiarg-element s (list b r))]))))))

(define chinese
  (lambda [#:font [font "FandolSong"] #:latex? [latex? 'auto] . contents]
    (cond [(symbol? latex?)
           (make-traverse-element
            (λ [get set!]
              (chinese #:font font #:latex? (handbook-latex-renderer? get)
                       contents)))]
          [(and latex?) (make-multiarg-element "Chinese" (list font contents))]
          [else contents])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-form
  (lambda contents
    (apply tt contents)))

(define ja-tabular2
  (lambda [table-head table-rows [empty-cols (list "" "")] [gap 1]]
    (define col-size (length table-head))
    (define col-properties (make-list col-size 'left))
    (define cel-properties (make-list col-size '()))
     
    (tabular #:sep (hspace gap)
             #:style 'block
             #:row-properties '(bottom-border ())
             #:column-properties (append col-properties '(center) col-properties)
             #:cell-properties (list (append cel-properties '(left-border) cel-properties))
             (cons (append table-head empty-cols table-head)
                   (let make-table ([rows table-rows]
                                    [swor null])
                     (cond [(null? rows) (reverse swor)]
                           [(null? (cdr rows)) (reverse (cons (append (car rows) empty-cols (make-list col-size 'cont)) swor))]
                           [else (make-table (cddr rows) (cons (append (car rows) empty-cols (cadr rows)) swor))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-input
  (lambda [word]
    (cond [(string? word) word]
          [(symbol? word) (symbol->string word)]
          [else (~a word)])))

(define ja-inputs
  (lambda words
    (apply values (map ja-input words))))

(define ja-example->ruby-token
  (lambda [token]
    (define content (ja-input token))

    (cond [(regexp-match? #px"[A-Z]+" content) (cons (tech (tt content)) "exmptag")]
          [(eq? token '-) (cons #false "ruby")]
          [else (cons content 'auto)])))

(define ja-ruby-content
  (lambda [v]
    (cond [(string? v) (string-split v "|")]
          [(list? v) v]
          [else (list v)])))

(define ja-hiragana?
  (lambda [ch]
    (char<=? #\u3040 ch #\u309F)))

(define ja-katakana?
  (lambda [ch]
    (char<=? #\u30A0 ch #\u30FF)))

(define ja-kigou?
  (lambda [ch]
    (char<=? ch #\rubout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-terminology-head 'literacy:japanese:terminology:head)
(define ja-terminology-rows 'literacy:japanese:terminology:rows)

(define ja-terminology-dictionary
  (lambda [get]
    (or (get ja-terminology-rows #false) (make-immutable-hash))))

(define ja-terminology
  (case-lambda
    [(get set! key en0 ja ch latex?)
     (define en (singular (string-titlecase en0)))
     
     (unless (get ja-terminology-head #false)
       (set! ja-terminology-head (list (bold "English") (bold "日本語") (chinese (bold "简体中文") #:latex? latex?))))
     
     (set! ja-terminology-rows
           (hash-set (or (get ja-terminology-rows #false)
                         (make-immutable-hash))
                     (or key en)
                     (list (string->symbol en)
                           (tech #:key key en)
                           ja
                           (if (not ch)
                               ja
                               (chinese (ja-input ch) #:latex? latex?)))))]
    [(get)
     (values (get ja-terminology-head #false)
             (map cdr (sort #:key car (hash-values (ja-terminology-dictionary get)) symbol<?)))]))

(define ja-terminology-abbreviation
  (lambda [get]
    (define dictionary (ja-terminology-dictionary get))
    (define abbreviation (hash-copy dictionary))

    (for ([term (in-hash-values dictionary)])
      (hash-ref! abbreviation (symbol->string (car term)) term))
    
    abbreviation))

(define ja-terminology-translation
  (lambda [term]
    (cddr term)))
