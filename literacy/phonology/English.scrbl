#lang scribble/book

@require{../literacy.rkt}
@require{../ipa.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define gap (hspace 1))
@(define gap2 (hspace 2))

@(define en-sentence
   (lambda [bases symbols]
     @nested[#:style tamer-boxed-style]{@ipa-ruby[bases symbols].}))

@(define en-words
   (lambda [bases symbols [cols 4]]
     (define bsize (length bases))
     (define ssize (length symbols))
     
     @nested[#:style tamer-boxed-style
             (tabular #:sep (hspace 4)
                      #:column-properties (make-list cols 'center)
                      (let tabulize ([ws bases]
                                               [ss (cond [(< ssize bsize) (append symbols (make-list (- bsize ssize) '-))]
                                                         [(> ssize bsize) (take symbols bsize)]
                                                         [else symbols])]
                                               [sowr null])
                        (cond [(< (length ws) cols)
                               (reverse (cons (append (map ipa-ruby ws ss)
                                                      (make-list (- cols (length ws)) ""))
                                              sowr))]
                              [else (let-values ([(wrow wrest) (split-at ws cols)]
                                                 [(srow srest) (split-at ss cols)])
                                                (tabulize wrest srest
                                                          (cons (map ipa-ruby wrow srow)
                                                                sowr)))])))]))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{(British) English Phonology}

@handbook-scenario{Prosody}

@handbook-action{Lexical Stress}

English employs the @ja-tech{stress accent} system to cue its prominent syllable in a word.
A word contains one or more syllables, among those there always is a primary stress; for some words,
especially for ones that conntain multiple syllables, a secondary stress might be identified; the
other syllables are considered unstressed@handbook-footnote{Here only described the phonemic stress
 despite the phonetic tertiary stress along with treating the unstressed as the quaternary one.}.

In the @ja-tech{IPA} transcriptions, the primary stress and secondary stress are denoted with
@ipa-phonetics{@ipa-sym{^}} and @ipa-phonetics{@ipa-sym{.}}@handbook-footnote{Tranditionally,
 English words may be acuted and graved for stresses: @emph{pronùnciátion}.} before the target
syllables, respectively, like in @ipa-phonetics{@ipa-/sym/{.6p*^tju:n*ti}}.

The positions of stressed syllables of English words are generally unpredictable, nonetheless, some
cheatsheets can be made for heuristic before looking up dictionaries.

@handbook-event{Disyllables}

@itemlist[
 @item{A @ja-tech{noun} or a @ja-tech{adjective} tends to stress on the first syllable.

  @en-sentence['(The artist^s most famous  picture shows some women and children in a lovely  forest with a  purple mountain behind)
               '(-   ^A:tIsts -   ^feIm*s  ^pIktS* -     -   ^wImIn -  ^tSIldr*n -  - ^l2vli ^f6rIst -    - ^p*:p*l ^maUntIn -)]}

 @item{A @ja-tech{verb} tends to stress on the second syllable, or on the one identified with its stem.

  @en-words['(contrast    export   present  rebel  remove dislike   rebuild recycle)
            '(k*n^trA:st Ik^spO:t prI^zent rI^bel rI^mu:v dis^laIk ri:^bIld ri:^saIk6l)
            4]

  Note that these two principles give an hint of how to speak a word if it can serve as
  both a @ja-tech{noun} and a @ja-tech{verb}, and also note that the pronunciations may
  be slightly different.
  
  @en-words['(record  "" record   contrast  "" contrast   export  "" export  present "" present  rebel "" rebel)
            '(^rekO:d -  rI^kO:d ^k6ntra:st - k*n^trA:st ^ekspO:t - Ik^spO:t ^prez*nt - prI^zent ^reb*l - rI^bel)
            6]}
                                                                   
 @item{Deriving a disyllabic word ususlly does not change the position of its stressed syllable.

  @en-words['(happy    ⇒  unhappy   depart   ⇒ departure)
            '(^h&ae$pi - 2n^h&ae$pi dI^pA:t  -  dI^pA:tS*)
            6]}

 @item{Irregularities:

  @itemlist[
 @item{@ja-tech{Nouns} and @ja-tech{adjectives} that stressing on the second syllable:
    @litchar{asleep}, @litchar{mistake}, @litchar{machine}, @litchar{alone}, @etc}

 @item{Words that serving as both @ja-tech{nouns} and @ja-tech{verbs} consistently stress
    on the first syllable despite the @ja-tech[#:key "PoS"]{parts of speech}: @litchar{promise},
    @litchar{answer}, @litchar{travel}, @litchar{visit}, @litchar{picture}, @etc}]}
 ]

@handbook-event{Polysyllable}

@itemlist[
 @item{Irregularities:

  @itemlist[
 @item{@litchar{-y}-suffiexed polysyllabic words tend to stress on the last third syllable:
    @en-words['(public  ⇒ publicity  photograph   ⇒ photography)
              '(^p2blIc - p2b^lIcIti ^f*Ut*grA:f  -  f*^t6gr*fi)
              6]}]
  }
 ]

@handbook-event{Compound Words}

@en-words['(airport lipstick  newspaper  Thanksgiving   driving  licence  unforgettable)
          '(^e*pO:t ^lIpstIk ^nju:zpeIp* T&ae$NksgIvIN ^draIvIN ^laIs*ns 2nf*^get*bl)
          8]

@handbook-action{Prosodic Stress}

.

@handbook-scenario{Materials for Practice}

@handbook-action{The Miracle Cat}

@tabular[
 #:sep gap2

 (list (list @ipa-ruby['(Wha=t do   you   call    yourself?)
                       '(w6t   du:  ju:   kO:l    jO:^self)
                       '(12345 5311 12345 6789987 76543210)])

       (list @ipa-ruby['(Alice.)
                       '(^&ae$lIs)
                       '(987654)])

       (list @ipa-ruby['(Th/e    A/lice?)
                       '(Di:     ^&ae$lIs)
                       '(8888876 5556789)])

       (list @ipa-ruby['(There^_s been _some deba/te a/bou=t that.)
                       '(De@z     bIn   s@m  dI^beIt *^baUt  D&ae$t)
                       '(4555555 44332 21111 1234555 5432112 34554432)])
 
       (list @ipa-ruby['(I  never ge/t i/nvolve/d i/n politics.)
                       '(aI ^nev@ get  In^v6lvd   In  p6l@tiks)
                       '(15 55554 333  34555554   433 3455443321)])

       (list @ipa-ruby['(You^=d bes=t b/e o/n your way.)
                       '(ju:d   best  bi  6n  jO: weI)
                       '(12345  55543 21  11  1123 3455)])
       
       (list @ipa-ruby['(Wha=t way?)
                       '(w6t   weI)
                       '(5554  4321)])
       
       (list @ipa-ruby['(A/ll I/ wan=t _to d/o +wi/s wa/ke u/=p _from this dream.)
                       '(O:l  aI  w6nt  t@ du:  Iz  weIk   2p   fr@m DIs  dri:m)
                       '(5554 32  223  34 4554  4321 123   455  54321 1112 3455432)])
       
       (list @ipa-ruby['(Fine. I^ll ta/ke you/ _to the Ha/re a/n=d the Hatter.)
                       '(faIn  aIl  teIk  yu:   t@ D@   he@  *nd    D@ ^h&ae$t@)
                       '(4555  543  3554  433  33  334 45555 543   21  1354321)])
       
       (list @ipa-ruby['(_Bu=t that^s th/e +je/n/d of i/t.)
                       '(b@t   D&ae$ts Di:   end  *v  It)
                       '(334   45555  543  355430   42 221)]))]

@handbook-action{The Blue Whale}

@tabular[
 #:sep gap

 (list (list @ipa-ruby['(The        open      ocean.)
                       '(Di:       ^*Up*n     *UpS*n)
                       '(5555567 88887766555 57788776655443)])

       (list @ipa-ruby['(It    covers     more  _than       half  _the   surface      _o_/f  our/  planet.)
                       '(It    k2v*z      mO:   D*n         hA:f  D*     ^s3:fIs       *v    aU*   ^pl&ae$nIt)
                       '(55556 678888767 789999 8877665544 789999 987777 788999998765 66665 554445 77665544334455)])

       (list @ipa-ruby['(Yet|,| _for  _the      mos=t      part|,|    it^s   a   watery     desert|,|      empt/y   _+jo/f life.)
                       '(jet     f*    D*       m*Ust      pA:t       Its    *   ^wO:t*ri   ^dez*t         ^empti    *v    laIf)
                       '(777777 5555 555567 778899998877 6788776543  666666 654 45677765432 24776655443311 67887654 234567 88876543)])

       (list @ipa-ruby['(Hunters here spen=d their live/s in a/ constan=t search   for scarce an=d illusive prey.)
                       '(^h2nt*z hI*  spend  De*   laIz   In *  ^k6nst*nt  s3:tS   f*  ske*s  *nd  I^lu:sIv preI)
                       '(999987  765  567776 6545  678887 67 765 5677765  5777653 4554 468887 4333 35776544 567531)])

       (list (para @ipa-ruby['(Remarkably|,| _this seemingly   barren  wilderness _is home  _to _the larges=t hunter  _of the/m a/ll --- _The blue whale.)
                             '(rI^mA:k*bli    DIs  ^si:mINli ^b&ae$r*n ^wIld*n*s  Iz  h*Um   t*  D*  lA:dZIst  ^h2nt* *v   D*m  O:l   -   D*  blu: weIl)
                             '(55678876543   55556 78877667   78999876 67766555   45 5566776 44 456  788888   8987654 55  557   987  654 2223 5677 76543210)])))]

@handbook-reference[]
