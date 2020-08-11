#lang scribble/book

@require{../literacy.rkt}
@require{../ipa.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define gap (hspace 1))
@(define gap2 (hspace 2))
@(define gap4 (hspace 4))

@(define en-sentence
   (lambda [bases symbols . contents]
     @nested[#:style tamer-boxed-style
             @|gap4| @ipa-ruby[bases symbols] "."
             contents]))

@(define en-words
   (lambda [bases symbols [cols 4]]
     (define bsize (length bases))
     (define ssize (length symbols))
     
     @nested[#:style tamer-boxed-style
             (tabular #:sep gap4
                      #:column-properties (make-list cols 'center)
                      (let tabulize ([ws bases]
                                     [ss (cond [(< ssize bsize) (append symbols (make-list (- bsize ssize) '-))]
                                               [(> ssize bsize) (take symbols bsize)]
                                               [else symbols])]
                                     [sowr null])
                        (cond [(null? ws) (reverse sowr)]
                              [(< (length ws) cols)
                               (reverse (cons (append (map ipa-ruby ws ss) (make-list (- cols (length ws)) ""))
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
 English words may be acuted and graved for stresses: @emph{pronùnciátion}.} before target
syllables, respectively, like in @ipa-phonetics{@ipa-/sym/{pr*.n2nsI^eIS*n}}.

Stressed syllables of English words are generally unpredictable, nonetheless, some cheatsheets
can be made for heuristic before looking up dictionaries.

@handbook-event{Disyllables}

@itemlist[
 @item{A @ja-tech{noun} or a @ja-tech{adjective} tends to stress on the first syllable.

  @en-sentence['(The artist^s most famous  picture shows some women and children in a lovely  forest with a  purple mountain behind)
               '(-   ^A:tIsts -   ^feIm*s  ^pIktS* -     -   ^wImIn -  ^tSIldr*n -  - ^l2vli ^f6rIst -    - ^p*:p*l ^maUntIn -)]}

 @item{A @ja-tech{verb} tends to stress on the second syllable, or on the one identified with its stem.

  @en-words['(contrast    export   present  rebel  remove dislike   rebuild recycle)
            '(k*n^trA:st Ik^spO:t prI^zent rI^bel rI^mu:v dis^laIk ri:^bIld ri:^saIk6l)
            8]

  Note that these two principles give an hint of how to speak a word if it can serve as
  both a @ja-tech{noun} and a @ja-tech{verb},

  @en-words['(insult  insult ""  transfer     transfer  "" discount  discount  "")
            '(^Ins2lt I^s2lt - ^tr&ae$nsf3: tr&ae$sn^f3: - ^dIskaUnt dIs^kaUnt - )
            9]
  
  and also note that the pronunciations may be slightly different.
  
  @en-words['(record  record  "" contrast   contrast ""  export   export "" present  present ""  rebel rebel "" project   project "")
            '(^rekO:d rI^kO:d - ^k6ntra:st k*n^trA:st - ^ekspO:t Ik^spO:t - ^prez*nt prI^zent - ^reb*l rI^bel - ^pr6dZekt pr*^dZekt -)
            9]}
                                                                   
 @item{Deriving a disyllabic word ususlly does not change its stressed syllable.

  @en-words['(happy    ⇒  unhappy   "" depart   ⇒ departure)
            '(^h&ae$pi - 2n^h&ae$pi  - dI^pA:t  -  dI^pA:tS*)
            8]}

 @item{Irregularities:

  @itemlist[
 @item{@ja-tech{Nouns} and @ja-tech{adjectives} that stressing on the second syllable:
    @litchar{asleep}, @litchar{mistake}, @litchar{machine}, @litchar{alone}, @etc}

 @item{Words that serving as both @ja-tech{nouns} and @ja-tech{verbs} consistently stress
    on the first syllable despite their @ja-tech[#:key "PoS"]{parts of speech}: @litchar{promise},
    @litchar{answer}, @litchar{travel}, @litchar{visit}, @litchar{picture}, @etc}]}
 ]

As irregularities are regular in language, mastering exact lexical accent of words therefore
is indispensable for high quality communicating with native speakers.

@en-sentence['(I must tell you|,| I^m /unique/)
             '(-  -    -    -      -  ju:^ni:k)]

@en-sentence['(I must tell you|,| I^m a /eunuch/)
             '(-  -    -    -      -  - ^ju:n*k)]

@handbook-event{Polysyllable}

A polysyllabic word is usually derived from a shorter word by adding prefixes and/or suffixes:

@itemlist[
 @item{@litchar{-ion}/@litchar{-ian}/@litchar{-tion}/@litchar{-cian}: stressed on the syllable
  previous to the suffix(the second last syllable).
  
  @en-words['(educate  ⇒ education    "" electric   ⇒  electrician "" decorate   ⇒ decoration "" communicate   ⇒ communication)
            '(^edjUkeIt - edjU^keIS*n  - I^lektrIk  -  .Ilek^trIS*n -  ^dek*reIt -  dek*^reIS*n - k*^mju:nIkeIt - k*.mju:^nIkeIS*n)
            8]

  By the way, @litchar{-tion} is realized as @ipa-phonetics{@ipa-/sym/{tS*n}} if following a
  @litchar{s}, or @ipa-phonetics{@ipa-/sym/{S*n}} otherwise; @litchar{-sion} is realized as
  @ipa-phonetics{@ipa-/sym/{S*n}} if following a @ja-tech{consonant}, or
  @ipa-phonetics{@ipa-/sym/{Z*n}} if following a @ja-tech{vowel}; specifically, @litchar{-ssion}
  is realized as @ipa-phonetics{@ipa-/sym/{S*n}}, that is, the leading @litchar{s} does not
  contribute to the pronunciation.
  
  @en-words['(suggestion  adoption extension  decision possession)
            '(s*^dZestS*n *^d6pS*n Iks^tenS*n dI^sIZ*n p*^zenS*n)
            8]}

@item{@litchar{-ic}/@litchar{-ial}/@litchar{-ive}: stressed on the syllable previous to the suffix.
       
  @en-words['(scientist  ⇒ scientific  "" economy   ⇒  economic ""    atom   ⇒ atomic  "" instinct ⇒ instinc/tive/)
            '(^saI*ntIst - s2I*n^tIfik  - I^c6n*mi  -  I:c*^n6mik -  ^&ae$t*m -  *^t6mik - ^InstINkt - In^stINktIv)
            8]}

@item{@litchar{-able}/@litchar{-al}: usually unchanged, or unpredictable (true irregular).
       
  @en-words['(admire  ⇒ admirable   "" prefer ⇒  preferable "" medicine ⇒ medicinal "" agriculture   ⇒ agricultural)
            '(*d^maIe - ^&ae$m*r*b*l - prI^f3: - ^pref*r*b*l - ^meds*n  -  mI^dIsIn*l - ^&ae$grIk2ltS* - &ae$grI^k2ltS*r*l)
            8]}

 @item{@litchar{-ious}/@litchar{-ulous}/@litchar{-orous}/@litchar{-eous}: stressed on the syllable
  previous to the suffix. Note that these suffixes will never be realized with @ipa-phonetics{@ipa-/sym/{r}}. 
       
  @en-words['(industry  ⇒ industrious "" mystery  ⇒ mysterious "" miracle ⇒   miraculous  ""  outrage  ⇒ outrageous)
            '(^Ind*stri - In^d2strI*s  - ^mIst*ri - mI^stI*rI*s - ^mIr*k*l - mI^r&ae$kjUl*s - ^aUtreIdZ - aUt^reIdZ*s)
            8]}
 
 @item{@litchar{-ee}/@litchar{-eer}/@litchar{-ese}/@litchar{-ette}: stressed on suffix itself.
  These suffixes are typically loaned from French, and their own stressed syllables shadow the ones of word stems. 
       
  @en-words['(engineer  cigarette  refugee   Chinese)
            '(endZI^nIe sIg*^ret  refjU^dZi: tSaI'ni:z)
            4]}

 @item{@litchar{-y}: stressed on the third last syllable.
  @en-words['(public  ⇒ publicity  "" photograph   ⇒ photography "" national   ⇒ nationality     "" author ⇒ authority)
            '(^p2blIc - p2b^lIcIti  - ^f*Ut*grA:f  -  f*^t6gr*fi - ^n&ae$S*n*l - n&ae$S*^n&ae$lIti - ^O:T* -  ^O:T6rIti)
            8]}
 ]

Otherwise, the stressed syllable tends to be remained:

@en-words['(economic   ⇒ economical  "" author ⇒ authorize "" regard  ⇒ regardless)
          '(I:c*^n6mik - I:c*^n6mik*l - ^O:T* - O:^T6raIz   - rI^gA:d - rI^gA:dl*s)
          8]

with irregularities:

@en-words['(advertise    ⇒   advertisement)
          '(^&ae$dv*taIz - &ae$d^v*taIzm*nt)
          8]

@handbook-event{Compound Words}

@en-words['(airport lipstick  newspaper  Thanksgiving   |driving  licence|  unforgettable)
          '(^e*pO:t ^lIpstIk ^nju:zpeIp* T&ae$NksgIvIN  |^draIvIN ^laIs*ns| 2nf*^get*bl)
          6]

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
