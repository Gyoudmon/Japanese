#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@(define-syntax (ja-deftech-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [abbr. en-col col ... zh-col] ...]
      #'(make-delayed-block
         (λ [render% pthis infobase]
           (define latex? (handbook-latex-renderer? render%))
           (ja-tabular2 (list (bold (symbol->string 'head)) ... (chinese (bold (symbol->string 'head-zh-col)) #:latex? latex?))
                        (map (λ [a] (cons (deftech (symbol->string (car a)) #:style? #false) (cdr a)))
                             (ja-terminology-abbreviations (handbook-resolved-info-getter infobase)
                                                           (list (list 'abbr.
                                                                       (tech (symbol->string 'en-col))
                                                                       (symbol->string 'col) ...
                                                                       (chinese (symbol->string 'zh-col) #:latex? latex?))
                                                                 ...)))))))))

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
 [PST    Past                      過去　　　　过去时]]}

@handbook-reference[]
