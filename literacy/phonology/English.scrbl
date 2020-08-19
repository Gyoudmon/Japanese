#lang scribble/book

@require{../literacy.rkt}
@require{../ipa.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define gap (hspace 1))
@(define gap2 (hspace 2))
@(define gap4 (hspace 4))

@(define en-letters
   (lambda [lttr]
     (litchar lttr)))

@(define en-phonetics
   (lambda [sym]
     (ipa-phonetics (ipa-/sym/ sym))))

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

@handbook-scenario{Phones}

@handbook-scenario{Phonemes}

@handbook-action{Voiceless and Voiced Morphological Suffixes}

@itemlist[
 @item{@en-letters{-ed} that of an inflected @ja-tech{verb} is realized as @en-phonetics{t} if
  following a @ja-tech{voiceless consonant}, and realized as @en-phonetics{d} if following a
  @ja-tech{voiced consonant} or a @ja-tech{vowel}. Specifically, @en-letters{-ed} is realized
  as @en-phonetics{Id} if following another @en-phonetics{t} or @en-phonetics{d}.

 @en-words['(asked finished helped passed reached talked launched watched)
           '(A:skt ^fInISt  helpt  pA:st  ri:tSt  tO:kt  lO:ntSt  w6tSt)
           8]

 @en-words['(called borrowed moved enjoyed  welcomed answered bridged shared)
           '(kO:ld  ^b6r*Ud  mu:vd In^dZOId ^welk*md ^A:ns*d  brIdZd  Se*d)
           8]
 
 @en-words['(wanted doubted concentrated  ended  decided    acted  counted sided)
           '(w6ntId daUtId  ^k6nsntreItId endId dI^saIdId &ae$ktId kaUntId SaIdId)
           8]

  Some @ja-tech{adjective}s may just end with @en-letters{ed}s, which are realized as @en-phonetics{Id}s.
  Do not be confused with the suffix @en-letters{-ed} of an inflected @ja-tech{verb}.
  
 @en-words['(beloved  wretched wicked  sacred    ragged   naked  krooked dogged cursed  learned -legged aged)
           '(bI^l2vId ^retSId  ^wIkId ^seIkrId ^r&ae$gId ^neIkId ^krUkId ^d6gId ^k3:sId 'l3:nId -legId ^eIdZId)
           6]}

 @item{@en-letters{-s}/@en-letters{-es} that of an inflected @ja-tech{noun} or @ja-tech{verb} is
  realized as @en-phonetics{s} if following a @ja-tech{voiceless consonant}, and realized as
  @en-phonetics{z} if following a @ja-tech{voiced consonant} or a @ja-tech{vowel}. Specifically,
  when following another @en-phonetics{t} or @en-phonetics{d}, they are combined as the consonant
  pair @en-phonetics{ts} or @en-phonetics{dz}. When following another @en-phonetics{s}, @en-phonetics{z},
  @en-phonetics{S}, @en-phonetics{Z}, @en-phonetics{tS}, or @en-phonetics{dZ},
  @en-letters{-s}/@en-letters{-es} is realized as @en-phonetics{Iz}; When inflected from
  @en-letters{f}/@en-letters{fe}, @en-letters{-s}/@en-letters{-es} is realized as @en-phonetics{vz}.
  
  
 @en-words['(dates pets books hopes talks asks  laughs cloth/s/)
           '(deIts pets bUks  heUps tO:ks A:sks  lA:fs  kl6Ts)
           8]

 @en-words['(codes tells sides teams turns breathe/s/ goods cloth/es/)
           '(k*Udz tellz saIds ti:mz t3:nz   bri:Dz   gUdz  kl*UDz)
           8]
 
 @en-words['(bushes     garages   surprises  |the witch's watches| |the nurse's purses|    |Max's faxes|)
           '(bUSIz  ^g&ae$rA:dZIz s*^praIzIz  |D* wItSIz w6tSIz|    |D* n3:sIs p3:sIs| |m&ae$ksIs f&ae$ksIs|)
           6]
 
 @en-words['(knife ⇒ knives leaf ⇒ leaves)
           '(kaIf  - kaIvz li:f - li:vz)
           6]}
 ]

@handbook-action{Aspiration}

@handbook-action{Lateral Release and Nasal Release}

@handbook-scenario{Prosody}

@handbook-action{Lexical Stress}

English employs the @ja-tech{stress accent} system@handbook-footnote{Linguistic terms are always
 the first source of chaos. In practice, English syllables might be emphasized in any way of:
 1). longer period; 2). heavier stress; or 3) higher pitch.} to cue its prominent syllable in a
word. A word contains one or more syllables, among those there always is a primary stress; for
some polysyllabic words, a secondary stress might be identified; the other syllables are considered
unstressed@handbook-footnote{Here only described the phonemic stress despite the  phonetic
 tertiary stress along with treating the unstressed as the quaternary one.}.

In the @ja-tech{IPA} transcriptions, the primary stress and secondary stress are denoted with
@ipa-phonetics{@ipa-sym{^}} and @ipa-phonetics{@ipa-sym{.}}@handbook-footnote{Tranditionally,
 English words may be acuted and graved for stresses: @emph{pronùnciátion}.} before target
syllables, respectively, like in @en-phonetics{pr*.n2nsI^eIS*n}.

Stressed syllables of English words are generally unpredictable, nonetheless, some cheatsheets
can be made for heuristic before looking up dictionaries.

@handbook-event{Disyllables}

@itemlist[
 @item{A @ja-tech{noun} or a @ja-tech{adjective} tends to stress on the first syllable.

  @en-sentence['(The artist^s most famous  picture shows some women and children in a lovely  forest with a  purple mountain behind)
               '(-   ^A:tIsts -   ^feIm*s  ^pIktS* -     -   ^wImIn -  ^tSIldr*n -  - ^l2vli ^f6rIst -    - ^p3:p*l ^maUntIn -)]}

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
    @en-letters{asleep}, @en-letters{mistake}, @en-letters{machine}, @en-letters{alone}, @etc}

 @item{Words that serving as both @ja-tech{nouns} and @ja-tech{verbs} consistently stress
    on the first syllable despite their @ja-tech[#:key "PoS"]{parts of speech}: @en-letters{promise},
    @en-letters{answer}, @en-letters{travel}, @en-letters{visit}, @en-letters{picture}, @etc}]}
 ]

As irregularities are regular in language, mastering exact lexical accent of words therefore
is indispensable for high quality communicating with native speakers.

@en-sentence['(I must tell you|,| I^m /unique/)
             '(-  -    -    -      -  ju:^ni:k)]

@en-sentence['(I must tell you|,| I^m a /eunuch/)
             '(-  -    -    -      -  - ^ju:n*k)]

@handbook-event{Polysyllables}

A polysyllabic word is usually derived from a shorter one by adding prefixes and/or suffixes:

@itemlist[
 @item{@en-letters{-ion}/@en-letters{-ian}/@en-letters{-tion}/@en-letters{-cian}: stressed on the syllable
  previous to the suffix(the second last syllable).
  
  @en-words['(educate  ⇒ education    "" electric   ⇒  electrician "" decorate   ⇒ decoration "" communicate   ⇒ communication)
            '(^edjUkeIt - edjU^keIS*n  - I^lektrIk  -  .Ilek^trIS*n -  ^dek*reIt -  dek*^reIS*n - k*^mju:nIkeIt - k*.mju:^nIkeIS*n)
            8]

  By the way, @en-letters{-tion} is realized as @en-phonetics{tS*n} if following a @en-letters{s},
  or @en-phonetics{S*n} otherwise; @en-letters{-sion} is realized as @en-phonetics{S*n} if following
  a @ja-tech{consonant}, or @en-phonetics{Z*n} if following a @ja-tech{vowel}; specifically,
  @en-letters{-ssion} is realized as @en-phonetics{S*n}, that is, the leading @en-letters{s}
  does not contribute to the pronunciation.
  
  @en-words['(suggestion  adoption extension  decision possession)
            '(s*^dZestS*n *^d6pS*n Iks^tenS*n dI^sIZ*n p*^zeS*n)
            8]}

@item{@en-letters{-ic}/@en-letters{-ial}/@en-letters{-ive}: stressed on the syllable previous to the suffix.
       
  @en-words['(scientist  ⇒ scientific  "" economy   ⇒  economic ""    atom   ⇒ atomic  "" instinct ⇒ instinc/tive/)
            '(^saI*ntIst - s2I*n^tIfik  - I^c6n*mi  -  I:c*^n6mik -  ^&ae$t*m -  *^t6mik - ^InstINkt - In^stINktIv)
            8]}

@item{@en-letters{-able}/@en-letters{-al}: usually unchanged, or unpredictable (true irregular).
       
  @en-words['(admire  ⇒ admirable   "" prefer ⇒  preferable "" medicine ⇒ medicinal "" agriculture   ⇒ agricultural)
            '(*d^maIe - ^&ae$m*r*b*l - prI^f3: - ^pref*r*b*l - ^meds*n  -  mI^dIsIn*l - ^&ae$grIk2ltS* - &ae$grI^k2ltS*r*l)
            8]}

 @item{@en-letters{-ious}/@en-letters{-ulous}/@en-letters{-orous}/@en-letters{-eous}: stressed on the syllable
  previous to the suffix. Note that these suffixes will never be realized with @en-phonetics{r}. 
       
  @en-words['(industry  ⇒ industrious "" mystery  ⇒ mysterious "" miracle ⇒   miraculous  ""  outrage  ⇒ outrageous)
            '(^Ind*stri - In^d2strI*s  - ^mIst*ri - mI^stI*rI*s - ^mIr*k*l - mI^r&ae$kjUl*s - ^aUtreIdZ - aUt^reIdZ*s)
            8]}
 
 @item{@en-letters{-ee}/@en-letters{-eer}/@en-letters{-ese}/@en-letters{-ette}: stressed on suffix itself.
  These suffixes are typically loaned from French, and their own stressed syllables shadow the ones of word stems. 
       
  @en-words['(engineer  cigarette  refugee   Chinese)
            '(endZI^nIe sIg*^ret  refjU^dZi: tSaI^ni:z)
            4]}

 @item{@en-letters{-y}: stressed on the third last syllable.
  @en-words['(public  ⇒ publicity  "" photograph   ⇒ photography "" national   ⇒ nationality     "" author ⇒ authority)
            '(^p2blIc - p2b^lIcIti  - ^f*Ut*grA:f  -  f*^t6gr*fi - ^n&ae$S*n*l - n&ae$S*^n&ae$lIti - ^O:T* -  O:^T6rIti)
            8]}
 ]

Otherwise, the stressed syllable tends to be remained:

@en-words['(forget ⇒ unforgettable ""  economic  ⇒ economical  "" author ⇒ authorize ""  regard ⇒ regardless)
          '(f*^get -  2nf*^get*bl   - I:c*^n6mik - I:c*^n6mik*l -  ^O:T* - ^O:T6raIz   - rI^gA:d - rI^gA:dl*s)
          8]

with irregularities:

@en-words['(advertise    ⇒  advertisement)
          '(^&ae$dv*taIz - &ae$d^v*tIzm*nt)
          8]

@handbook-event{Compound Words}

A compound word is the word composed of two or more single words and represents a new word. Compound
words can be written in any of the three forms: 1). a single word; 2). a hyphenated long word; or
3) raw form as if they were not compound.

@en-words['(airport lipstick  newspaper   Thanksgiving   |driving  licence| wil=d-life)
          '(^e*pO:t ^lIpstIk ^nju:zpeIp* T&ae$Nks^gIvIN  |^draIvIN laIs*ns| ^waildlaif)
          6]

The stress accent of a compound word depends on the @ja-tech{information}@handbook-footnote{This is
 really the key point for all prosody categories, not only for the lexical stress or compound word.}
either can be plausibly influenced or intended to emphasize. The stressed syllable of a compound word
is the syllable of the selected stressed word. For two-word compound word, not all two words should be
selected as stressed, if so they are not compounded and behave like two words in the sentence; For
longer compound word, no stressed word should be selected next to another stressed word, if so it is
an error.

Native speakers have made themselves very sensible to the stress accent patterns of compound words.

@en-words['(|green house| |[two words, a house colorzied with green]|)
          '(|^gri:n ^haUs|  -)
          2]

@en-words['(greenhouse |[plausibly inflenced, greenhouse is rarer than house]|)
          '(^gri:nhaUs  -)
          2]

@en-words['(|Italian Teacher| |[The teacher is an Italian]|)
          '(|I^t&ae$lj*n ti:tS*| -)
          2]

@en-words['(|Italian Teacher|  |[The teacher teaches Italian]|)
          '(|It&ae$lj*n ^ti:tS*| - )
          2]

Besides, some compound words may have more than one stressed words, these words may not be considered
as irregular, they are just not that into a single word.

@handbook-event{Reading: A Sad Birthday}

@tabular[
 #:sep gap

 (list (list @ipa-ruby['(It^s   my        birthday         _to_day.)
                       '(Its    maI       ^b3:TdeI         t*^deI)
                       '(6665 554433 357999887766554433 33445566554433)])

       (list @ipa-ruby['(An/d  I^m  e/xpecting    lo/ts a/n=d lo/ts    o/f cards.)
                       '(^nd   aIm  Ik^spektIN    l6ts  *nd   l6ts     *v  kA:dz)
                       '(34555 5554 3456788776655 57987 6556  78877665 56  6777665544)])

       (list @ipa-ruby['(Okay|,| here  we     go.    Newspaper|,|  a  letter  _to_  my     mum|,| fas=t food|,|  pizza|,| a letter _to_ my  dad|,| newspaper!)
                       '(.*U^keI hI*   wi     g*U    ^nju:s.peIp*  *  ^let*    t*   maI     m2m    fa:st fu:d    ^pi:ts*  * ^let*   t* maI  d&ae$d ^nju:z.peIp*)
                       '(999987 776655 443345 56776655 777777654357 545 6789998 7766 554433 445566  5555  56655443 88997755 54 567777 432 234 677654 8888877665544)])

       (list @ipa-ruby['(Bu=t  where +rare my    birthday   cards? I^_ve_ go=t no   cards.)
                       '(b*t    we*    A:  maI   ^b3:TdeI   kA:dz    aIv   g6t n*U  kA:dz)
                       '(65556 556677 776 65445 5678998767 788765 6666   6544 4565 56654)])
       
       (list @ipa-ruby['(Even    my     dog   ha=d  more   cards  than  m/e  +jon  =hi/s birthday.)
                       '(^i:ven  maI    d6g  h&ae$d mO:    kA:dz  D*n   mi:   6n    hIz   ^b3:TdeI)
                       '(6677876 6556 678998 98766 665544 444454 43334 4565 54456 678998 76543222)])
       
       (list @ipa-ruby['(Maybe   my       do/g     a/=t=e     my      cards.)
                       '(^meIbi  maI      d6g       eIt       maI     kA:dz)
                       '(5566776 654456 677766 6789988776655 56677 776655443322)])
       
       (list @ipa-ruby['(_But_|,| I don^/t a/ctually   ha/ve       a/    dog.)
                       '(b*t     aI doUnt  ^&ae$ktS*li h&ae$v      *     d6g)
                       '(6666   555 55676  67887766554 45678987 876788 888776655443322)]))]

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

       (list @ipa-ruby['(Remarkably|,| _this seemingly   barren  wilderness _is home  _to _the larges=t hunter  _of the/m a/ll --- _The blue whale.)
                       '(rI^mA:k*bli    DIs  ^si:mINli ^b&ae$r*n ^wIld*n*s  Iz  h*Um   t*  D*  lA:dZIst  ^h2nt* *v   D*m  O:l   -   D*  blu: weIl)
                       '(55678876543   55556 78877667   78999876 67766555   45 5566776 44 456  788888   8987654 55  557   987  654 2223 5677 76543210)]))]


@handbook-action{Peppa Pig}

@tabular[
 #:sep gap

 (list (list @ipa-ruby['(It^s   Gran=dda=d    Dog  _with_  Danny      Dog.)
                       '(Its  gr&ae$nd&ae$d   d6g   wID   ^d&ae$ni    d6g)
                       '(44445 5678886688867 77765  5556  678999758 88887654)])

       (list @ipa-ruby['(Hello|,| Peppa.)
                       '(h*^l*U   pep*)
                       '(99998765 655556789)])

       (list @ipa-ruby['(Hello|,| Danny.   _We_^re lost.)
                       '(h*^l*U  ^d&ae$ni    wI*   l6st)
                       '(99998765 655556789 555666 666543)])

       (list @ipa-ruby['(Lost?     I/s   y/our    sa=tnav    broken?     Satnav?)
                       '(l6st       Iz    jO:  ^s&ae$tn&ae$v ^br*Uk*n ^s&ae$tn&ae$v)
                       '(667898765 5555 569987    777777     7666679   99998765567)])

       (list @ipa-ruby['(You^re    drivi/ng   a/   camper    van    T   thirty  two hundred|,| sa=tnav    come/s a/ standard.)
                       '(jO:       ^draIvIN   *  ^k&ae$mp*   v&ae$n tI: ^T3:ti  tu: ^h2ndrId ^s&ae$tn&ae$v k2mz  *  ^st&ae$nd*d)
                       '(667776555  678998765 56  7888876557 76678 877 7899987 777 7899999753 899975334  456666 655 57998765)])

       (list @ipa-ruby['(Welcome  to  the  car   of the future.)
                       '(^welk*m  t*  D*   kA:   *v D* ^fju:tS*)
                       '(77776556 677 7776 65445 67  78 899876543)])

       (list @ipa-ruby['(Ah....  So  that^s  wha=t tha=t  button does.)
                       '(A:     s*U  D&ae$ts  w6t  D&ae$ ^b2t+nn d2z)
                       '(899996 5445 5678999 98876 655567 899998 76543)])

       (list @ipa-ruby['(Where   are we  going  today?)
                       '(we*     A:  wi  ^g*UIN t*^deI)
                       '(7999987 777 777 55555  55556789)])

       (list @ipa-ruby['(Tha=t    camper    va/n  i/s talking.)
                       '(D&ae$t ^k&ae$mp*  v&ae$n Iz  tO:kIN)
                       '(3344556 678998765  54445 579 9876543210)]))]

@handbook-reference[]
