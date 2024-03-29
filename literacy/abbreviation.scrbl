#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@;{
 WARNING:
  @deftech does not work in delayed blocks since all links are resolved before the render pass,
  @tech works in traverse blocks only if the target has already been defined before,
  thus, this chapter should be place in the appendix part of a book.
}

@(define-syntax (ja-deftech-table stx)
   (syntax-case stx []
     ([_ [head ... head-zh-col] [abbr. en-col col ... zh-col] ...]
      #'(make-traverse-block
         (λ [get set!]
           (define latex? (handbook-latex-renderer? get))
           (ja-tabular/2 (list (bold (symbol->string 'head)) ... (chinese (bold (symbol->string 'head-zh-col)) #:latex? latex?))
                         (map (λ [a] (cons (deftech (symbol->string (car a)) #:style? #false) (cdr a)))
                              (ja-terminology-abbreviations get
                                                            (list (list 'abbr.
                                                                        (tech (symbol->string 'en-col))
                                                                        (symbol->string 'col) ...
                                                                        (chinese (symbol->string 'zh-col) #:latex? latex?))
                                                                  ...)))
                         3))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{Abbreviation}

@centered{
 @ja-deftech-table[
 [Abbr.  English                   日本語   　简体中文]
 [ATT    Attributive               -         -]
 [COP    Copula                    -         -]
 [NEG    Negative                  -         -]
 [PASS   Passive                   -         -]
 [PLN    |Plain Form|              -         -]
 [POL    |Polite Form|             -         -]
 [TOP    |Topic Particle|          -         -]]}

@handbook-reference[]
