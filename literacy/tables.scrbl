#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@(define-syntax (ja-comparative-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [en-col col ... zh-col] ...]
      #'(tabular #:sep @hspace[1]
                 #:style 'block
                 #:column-properties '(center center center)
                 #:row-properties '(bottom-border ())
                 (list (list (bold (symbol->string 'head)) ... (bold (chinese (symbol->string 'head-zh-col))))
                       (list (tech (symbol->string 'en-col)) (symbol->string 'col) ... (chinese (symbol->string 'zh-col)))
                       ...)))))

@handbook-story[#:style noncontent-style]{Comparative Tables}

@ja-comparative-table[
 [English             日本語 　　汉语普通话]
 [|Independent Words| 自立語 　　实词]
 [|Ancillary Words|   付属語 　　虚词]
 [Particles            助詞　   助词]
 [Case　　　　　　　　　　格　　　　格]
 [|Accusative Case|　　対格　　　宾格]]

@handbook-reference[]
