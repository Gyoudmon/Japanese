#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@(define-syntax (ja-deftech-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [abbr. en-col col ... zh-col] ...]
      #'(ja-tabular2 (list (bold (symbol->string 'head)) ... (chinese (bold (symbol->string 'head-zh-col))))
                     (map (λ [a] (cons (deftech (symbol->string (car a)) #:style? #false) (cdr a)))
                          (sort #:key car
                                (list (list 'abbr.
                                            (tech (symbol->string 'en-col))
                                            (symbol->string 'col) ...
                                            (chinese (symbol->string 'zh-col))) ...)
                                symbol<?))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story[#:style noncontent-style]{Abbreviations}
  
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
 [SFP    |Sentence-Final Particle| 終助詞　　　语气助词]]}

@handbook-reference[]
