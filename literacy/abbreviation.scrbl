#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@(define make-abbreviation-row
   (case-lambda
     [(latex? abbr. en ja zh abbrs)
      (list abbr. (tech (symbol->string en)) (symbol->string ja) (chinese (symbol->string zh) #:latex? latex?))]
     [(latex? abbr. en abbrs)
      (define str-en (symbol->string en))
      (define maybe-term (hash-ref abbrs (symbol->string abbr.) (λ [] (hash-ref abbrs (singular (string-titlecase str-en)) (λ [] #false)))))
      (list* abbr. (tech str-en)
             (cond [(not maybe-term) (list "-" "-")]
                   [else (ja-terminology-translation maybe-term)]))]))

@(define-syntax (ja-deftech-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [abbr. en-col col ...] ...]
      #'(make-delayed-block
         (λ [render% pthis infobase]
           (define latex? (handbook-latex-renderer? render%))
           (define abbreviations (ja-terminology-abbreviation (handbook-resolved-info-getter infobase)))
           (ja-tabular2 (list (bold (symbol->string 'head)) ... (chinese (bold (symbol->string 'head-zh-col)) #:latex? latex?))
                        (map (λ [a] (cons (deftech (symbol->string (car a)) #:style? #false) (cdr a)))
                             (sort #:key car
                                   (list (make-abbreviation-row latex? 'abbr. 'en-col 'col ... abbreviations) ...)
                                   symbol<?))))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story[#:style noncontent-style]{Abbreviation}

@centered{
 @ja-deftech-table[
 [Abbr.  English                   日本語   　简体中文]
 [ABL    Ablative                  奪格　　　　夺格]
 [ACC    Accusative　　　　　　      対格   　　宾格]
 [DAT    Dative                    与格       与格]
 [INSTR  ﻿instrumental              具格       工具格]
 [NOM    Nominative　　　　　　      主格　　　　主格]
 [NP     |Noun Phrase|             名詞句     名词短语]
 [PST    Past                      過去　　　　过去时]
 [SFP    |Sentence-Final Particle|]]}

@handbook-reference[]
