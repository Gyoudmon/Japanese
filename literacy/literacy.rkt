#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)
(require digimon/system)
(require digimon/collection)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

(require (for-syntax syntax/parse))

(require "../digitama/realm.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just in case, README.md needs access examples
(enter-digimon-zone!)
(default-realm-paths (list (digimon-path 'realm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-example-index-type 'ex)
(define ja-example-style (make-style "jaexbox" (list 'command)))
(define ja-tech-style (make-style "jatech" null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (ja-title stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))
              (~optional (~seq #:abbr abbr) #:defaults ([abbr #'#false]))
              (~optional (~seq #:ja-term? term?) #:defaults ([term? #'#true])))
        ... en0 kanji0 (~optional hiragana0 #:defaults ([hiragana0 #'-])) (~optional zh #:defaults ([zh #'#false])))
     #'(let-values ([(en kanji hiragana) (ja-inputs 'en0 'kanji0 'hiragana0)])   
         (make-traverse-element
          (λ [get set!]
            (if (list? term?)
                (for ([t? (in-list term?)]
                      [t (if key (in-list (string-split key "|")) (in-cycle (in-value #false)))]
                      [e (in-list (string-split en "|"))]
                      [k (in-list (string-split kanji "|"))]
                      [h (in-cycle (in-list (string-split hiragana "|")))]
                      [a (if 'abbr (in-list (string-split 'abbr "|")) (in-cycle (in-value #false)))]
                      [z (if 'zh (in-list (string-split 'zh "|")) (in-cycle (in-value #false)))]
                      #:when t?)
                  (ja-terminology get set! t e k h z (handbook-latex-renderer? get) (and a (string->symbol a))))
                (unless (not term?)
                  (ja-terminology get set! key en kanji hiragana 'zh (handbook-latex-renderer? get) 'abbr)))
            (list (if (list? term?) (string-replace en "|" " ") en)
                  ~ "|" ~
                  (cond [(or (eq? 'hiragana0 '-) (equal? 'hiragana0 "-")) kanji]
                        [else (ruby kanji hiragana)])))))]))

(define-syntax (ja-deftech stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))
              (~optional (~seq #:abbr abbr) #:defaults ([abbr #'#true]))
              (~optional (~seq #:ja-term? term?) #:defaults ([term? #'#true])))
        ... en0 kanji0 (~optional hiragana0 #:defaults ([hiragana0 #'-])) (~optional zh #:defaults ([zh #'#false])))
     #'(let-values ([(en kanji hiragana) (ja-inputs 'en0 'kanji0 'hiragana0)])    
         (make-traverse-element
          (λ [get set!]
            (unless (not term?)
              (ja-terminology get set! key en kanji hiragana 'zh (handbook-latex-renderer? get) 'abbr))
            (list (deftech en #:key key)
                  ~ "「"
                  (cond [(or (eq? 'hiragana0 '-) (equal? 'hiragana0 "-")) kanji]
                        [else (ruby kanji hiragana)])
                  "」"))))]))

(define-syntax (ja-tech stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))) ...
        en0)
     #'(let ([en (ja-input 'en0)])
         (tech #:key (or key en)
               (elem #:style ja-tech-style en)))]))

(define-syntax (ja-tech* stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:key key) #:defaults ([key #'#false]))) ...
        en kanji hiragana)
     #'(tech #:key (or key (ja-inputs 'en))
             (ja-word #:style ja-tech-style
                      en kanji hiragana))]))

(define-syntax (ja-word stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #''tt]))) ...
        en0 kanji0 hiragana0)
     #'(let-values ([(en kanji hiragana) (ja-inputs 'en0 'kanji0 'hiragana0)])
         (elem #:style style
               en ~
               "「"
               (cond [(or (eq? 'hiragana0 '-) (equal? 'hiragana0 "-")) kanji]
                     [else (ruby kanji hiragana)])
               "」"))]
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #''tt]))) ...
        word)
     #'(racketcommentfont (elem #:style style "「" word "」"))]))

(define-syntax (ja-quote stx)
  (syntax-parse stx #:datum-literals []
    [(_ origin:str)
     #'(racketmetafont "「" (emph origin) "」")]))

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

(define-syntax (ja-example stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [[ja0 ja ...] [ruy ...] [trans ...] ...] ...)
     #'(let ([extag (or 'maybe-tag (gensym 'ex))])
          (ja-exemplify extag (list (realm-example (list (ja-input 'ja0) (ja-input 'ja) ...)
                                                   (list (ja-input 'ruy) ...)
                                                   (list (string-join (map ja-input '(trans ...)))
                                                         ...))  ...)))]
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [ja0 ja ...] [ruy ...] [trans ...] ...)
     #'(ja-example #:tag maybe-tag [[ja0 ja ...] [ruy ...] [trans ...] ...])]))

(define-syntax (ja-exref stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:elem ex-element:id) #:defaults ([ex-element #'subscript]))) ...
        extag)
     #'(ja-example-ref #:elem ex-element 'extag '||)]
    [(_ (~alt (~optional (~seq #:elem ex-element:id) #:defaults ([ex-element #'subscript]))) ...
        extag subtag)
     #'(ja-example-ref #:elem ex-element 'extag 'subtag)]
    [(_ (~alt (~optional (~seq #:elem ex-element:id) #:defaults ([ex-element #'subscript]))) ...
        extag subtag0 subtagn)
     #'(ja-example-ref* #:elem ex-element 'extag 'subtag0 'subtagn)]))

(define-syntax (ja-ExRef stx)
  (syntax-parse stx #:datum-literals []
    [(_ extag)
     #'(ja-example-ref #:elem values 'extag '||)]
    [(_ extag subtag)
     #'(ja-example-ref #:elem values 'extag 'subtag)]
    [(_ extag subtag0 subtagn)
     #'(ja-example-ref* #:elem values 'extag 'subtag0 'subtagn)]))

(define ja-exemplify
  (case-lambda
    [(id)
     (let ([sym:extag (tamer-indexed-block-id->symbol id)])
       (ja-exemplify sym:extag (exemplify sym:extag)))]
    [(sym:extag examples)
     (define excount (length examples))

     (case excount
       [(0)
        (ja-exemplify sym:extag (list (realm-example (list (ja-input sym:extag)) null null)))]
       [(1)
        (make-tamer-indexed-traverse-block
         #:latex-anchor 'example
         (λ [type chapter-index current-index]
           (define example-row (ja-example->table-row (car examples)))
           
           (values sym:extag
                   (tabular #:style 'block
                            #:column-properties '(left)
                            #:row-properties (append '(bottom-border) (make-list (sub1 (length example-row)) '()) '(bottom-border))
                            (append (list (list (tamer-indexed-block-elemtag #:type type #:tail #false sym:extag "Example" chapter-index current-index)))
                                    example-row))))
         ja-example-index-type
         ja-example-style)]
       [else
        (make-tamer-indexed-traverse-block
         #:latex-anchor 'example
         (λ [type chapter-index current-index]
           (define example-rows
             (for/list ([example-row (in-list (map ja-example->table-row examples))]
                        [sub (in-naturals 0)])
               (define subexample (string (integer->char (+ 97 sub))))
               (list (tabular #:sep (hspace 1)
                              #:style 'block
                              #:column-properties '(left center left)
                              (list (list ""
                                          (tamer-elemtag #:type type (~a sym:extag subexample) (envvar subexample))
                                          (tabular #:style 'block example-row)))))))
           
           (values sym:extag
                   (tabular #:style 'block
                            #:column-properties '(left)
                            #:row-properties (make-list (add1 (length example-rows)) 'bottom-border)
                            (append (list (list (list ($tex:phantomsection)
                                                      (tamer-indexed-block-elemtag #:type type #:separator #false sym:extag "Example" chapter-index current-index))))
                                    example-rows))))
         ja-example-index-type
         ja-example-style)])]))

(define ja-example-ref
  (lambda [#:elem [ex-element subscript] extag subtag]
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (if (not maybe-index)
           (racketerror (ex-element (~a type chapter-index #\. '? subtag)))
           (tamer-elemref #:type type (~a extag subtag) (racketresultfont (ex-element (~a type chapter-index #\. maybe-index subtag))))))
     ja-example-index-type extag)))

(define ja-example-ref*
  (lambda [#:elem [ex-element subscript] extag subtag0 subtagn]
    (make-tamer-indexed-block-ref
     (λ [type chapter-index maybe-index]
       (if (not maybe-index)
           (racketerror (ex-element (~a type chapter-index #\. '? subtag0 #\- subtagn)))
           (ex-element (tamer-elemref #:type type (~a extag subtag0) (racketresultfont (~a type chapter-index #\. maybe-index subtag0)))
                       "-"
                       (tamer-elemref #:type type (~a extag subtagn) (racketresultfont (~a subtagn))))))
     ja-example-index-type extag)))

(define ja-example->table-row
  (lambda [ex]
    (define ruby.styles (map ja-example->ruby-token (realm-example-rubies ex)))
    (list* (list (ruby (realm-example-tokens ex) (map car ruby.styles) #:style (map cdr ruby.styles)))
           ;;; NOTE:
           ;; Rubies that displayed under kanjis do not contribute to the height of whole,
           ;;  an empty line is required to avoid overlapping.
           (list "")
           (map (λ [t] (list (exec (list ~ t))))
                (realm-example-translations ex)))))

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

    (make-traverse-element
     (λ [get set!]
       (define latex? (handbook-latex-renderer? get))
       (for/list ([b0 (in-list bases)]
                  [i (in-naturals 0)])
         (define-values (b p) (ja-split b0))
         (define r (and (< i rsize) (list-ref rubies i)))
         (define s0 (if (< i ssize) (list-ref styles i) slast))
         (define s (cond [(not (eq? s0 'auto)) s0]
                         [else (let ([eidx (sub1 (string-length b))])
                                 (cond [(= eidx 0) "exmprubyvc"] ; 'auto is designed for examples 
                                       [else (let ([skana? (ja-kana? (string-ref b 0))]
                                                   [ekana? (ja-kana? (string-ref b eidx))])
                                               (cond [(eq? skana? ekana?) "exmprubyvc"]
                                                     [(not skana?) "exmprubyvl"]
                                                     [else "exmprubyvr"]))]))]))
         
         (cond [(or (not r) (eq? r '-) (eq? r '||) (equal? r "") (equal? r "-")) b0]
               [(not latex?) (list b (if (and (string? b) (ja-hiragana? (string-ref b 0))) (subscript r) #|<= why interchanged =>|# (superscript r)) (or p null))]
               [else (let ([br (make-multiarg-element (if (pair? options) (make-style s (list (make-command-optional (map ~a options)))) s) (list b r))])
                       (if (not p) br (list br p)))]))))))

(define chinese
  (lambda [#:font [font "FandolSong"] #:latex? [latex? 'auto] . contents]
    (cond [(symbol? latex?)
           (make-traverse-element
            (λ [get set!]
              (chinese #:font font #:latex? (handbook-latex-renderer? get)
                       contents)))]
          [(and latex?) (make-multiarg-element "Font" (list font contents))]
          [else contents])))

(define IPA
  (lambda [#:latex? [latex? 'auto] . contents]
    (cond [(symbol? latex?)
           (make-traverse-element
            (λ [get set!]
              (IPA #:latex? (handbook-latex-renderer? get)
                   contents)))]
          [(and latex?) (make-element "textipa" contents)]
          [else contents])))

(define tone
  (lambda [#:latex? [latex? 'auto] #:tone [latex-tone "tone"] pitch]
    (define pitch-seqs (~a pitch))
    (cond [(symbol? latex?)
           (make-traverse-element
            (λ [get set!]
              (tone #:latex? (handbook-latex-renderer? get) #:tone latex-tone
                    pitch-seqs)))]
          [(and latex?) (make-element latex-tone pitch-seqs)]
          [else (racketerror (format "\\~a{~a}" latex-tone pitch-seqs))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-form
  (lambda contents
    (racketmetafont "「" contents "」")))

(define ja-tabular2
  (lambda [table-head table-rows [gap 1] [empty-cols (list "")]]
    (define col-size (length table-head))
    (define col-properties (make-list col-size 'left))
    (define cel-properties (make-list col-size '()))
     
    (tabular #:sep (hspace gap)
             #:style 'centered
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

    (cond [(or (eq? token '-) (equal? token "-")) (cons #false "ruby")]
          [(not (regexp-match? #px"[A-Za-z]+" content)) (cons content 'auto)]
          [(not (regexp-match? #px"[./]" content)) (cons (tech (tt content)) "exmptag")]
          [else (cons (add-between (for/list ([subtoken (in-list (string-split content "."))])
                                     (cond [(not (string-contains? subtoken "/")) (tech (tt subtoken))]
                                           [else (add-between (map (compose1 tech tt) (string-split content "/")) "/")])) ".")
                      "exmptag")])))

(define ja-split
  (lambda [token]
    (define size (string-length token))

    (let search ([pos size])
      (cond [(<= pos 0) (values token #false)]
            [(ja-special-punctuation? (string-ref token (sub1 pos))) (search (sub1 pos))]
            [(= pos size) (values token #false)]
            [else (values (substring token 0 pos) (substring token pos size))]))))

(define ja-ruby-content
  (lambda [v]
    (cond [(list? v) v]
          [(string? v) (string-split v "|")]
          [else (list v)])))

(define ja-hiragana?
  (lambda [ch]
    (char<=? #\u3040 ch #\u309F)))

(define ja-katakana?
  (lambda [ch]
    (char<=? #\u30A0 ch #\u30FF)))

(define ja-kana?
  (lambda [ch]
    (or (ja-hiragana? ch)
        (ja-katakana? ch))))

(define ja-special-punctuation?
  (lambda [ch]
    (and (memv ch '(#\？ #\、 #\。 #\， #\? #\. #\, #\! #\！))
         #true)))

(define ja-kigou?
  (lambda [ch]
    (char<=? ch #\rubout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-terminology-head 'literacy:japanese:terminology:head)
(define ja-terminology-rows 'literacy:japanese:terminology:rows)
(define ja-terminology-abbrs 'literacy:japanese:terminology:abbrs)

(define ja-terminology
  (case-lambda
    [(get set! key en0 kanji ruby ch latex? abbr)
     (define en (singular (string-titlecase en0)))
     (define term
       (list (string->symbol en)
             (tech #:key key en)
             (cond [(or (eq? ruby '-) (equal? ruby "-")) kanji]
                   [(not (string-contains? kanji "|")) (make-multiarg-element "rubyterm" (list kanji ruby))]
                   [else (map (λ [k h] (if (or (string=? h "") (string=? h "-")) k (make-multiarg-element "rubyterm" (list k h))))
                              (string-split kanji "|")
                              (string-split ruby "|"))])
             (if (not ch)
                 kanji
                 (chinese (ja-input ch) #:latex? latex?))))
     
     (unless (get ja-terminology-head #false)
       (set! ja-terminology-head (list (bold "English") (bold "日本語") (chinese (bold "简体中文") #:latex? latex?))))
     
     (set! ja-terminology-rows
           (hash-set (or (get ja-terminology-rows #false)
                         (make-immutable-hash))
                     (or key en) term))
     
     (when (symbol? abbr)
       (set! ja-terminology-abbrs
             (hash-set (or (get ja-terminology-abbrs #false)
                           (make-immutable-hasheq))
                       abbr (cons abbr (cdr term)))))]
    [(get)
     (values (get ja-terminology-head #false)
             (map cdr (sort #:key car
                            (hash-values (or (get ja-terminology-rows #false)
                                             (make-immutable-hash)))
                            symbol<?)))]))

(define ja-terminology-abbreviations
  (lambda [get additions]
    (sort #:key car
          (append additions
                  (hash-values (or (get ja-terminology-abbrs #false)
                                   (make-immutable-hasheq))))
          symbol<?)))
