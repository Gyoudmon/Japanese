#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@(define-syntax (ja-deftech-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [abbr. en-col col ... zh-col] ...]
      #'(let ([table-head (list (bold (symbol->string 'head)) ... (chinese (bold (symbol->string 'head-zh-col))))]
              [empty-cell ""])
          (define terms
            (list (list (cond [(eq? '- 'abbr.) (aux-elem  "-")]
                              [else (deftech (symbol->string 'abbr.) #:style? #false)])
                        (tech (symbol->string 'en-col))
                        (symbol->string 'col) ...
                        (chinese (symbol->string 'zh-col))) ...))
          (tabular #:sep @hspace[1]
                   #:style 'block
                   #:column-properties '(left left left left center left left left left)
                   #:row-properties '(bottom-border ())
                   #:cell-properties (list '(() () () () left-border () () () ()))
                   (cons (append table-head (cons empty-cell table-head))
                         (let make-table ([terms terms]
                                          [swor null])
                           (cond [(null? terms) (reverse swor)]
                                 [(null? (cdr terms)) (reverse (cons (append (car terms) (list empty-cell empty-cell (and 'head 'cont) ...)) swor))]
                                 [else (make-table (cddr terms) (cons (append (car terms) (cons empty-cell (cadr terms))) swor))]))))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story[#:style noncontent-style]{Terminology}
  
@centered{
 @ja-deftech-table[
 [Abbr.  English                   日本語   　汉语普通话]
 [ABL    Ablative                  奪格　　　　夺格]
 [ACC    Accusative　　　　　　      対格   　　宾格]
 [-      |Ancillary Words|         付属語     虚词]
 [-      Case                      格       　格]
 [DAT    Dative                    与格       与格]
 [-      |Independent Words|       自立語  　 实词]
 [INSTR  ﻿instrumental              具格      工具格]
 [-      Particles                 助詞    　 助词]
 [NOM    Nominative　　　　　　      主格　　　　主格]
 [NP     |Noun Phrase|             名詞句     名词短语]
 [PST    Past                      過去　　　　过去时]
 [SFP    |Sentence-Final Particle| 終助詞　　　语气助词]]}

@handbook-reference[]
