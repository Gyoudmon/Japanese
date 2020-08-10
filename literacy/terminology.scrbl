#lang scribble/book

@(require digimon/tamer)

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{Terminology}

@centered{
 @(make-delayed-block
   (λ [render% pthis infobase]
     (define get (handbook-resolved-info-getter infobase))
     (define-values (table-head terms) (ja-terminology get))

     (cond [(not table-head) (tabular null)]
           [else (ja-tabular2 table-head terms)])))}

@handbook-reference[]
