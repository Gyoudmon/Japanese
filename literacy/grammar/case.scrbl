#lang scribble/book

@require{../literacy.rkt}
@require{../token.rkt}

@(define-book-bib ToC "The Handbook of Case" "Martin Haspelmath" "Oxford University Press"
   #:chapter "Terminology of Case"
   #:date 2006
   #:url "https://www.eva.mpg.de/fileadmin/content_files/staff/haspelmt/pdf/CaseTerminology.pdf")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title[Case 格 かく 格]}

A @deftech{case} is any of the forms of a @ja-tech{noun}, @ja-tech{adjective}, or @ja-tech{pronoun}
that express the semantic relation of a word to other words, especially to @ja-tech{verbs}, in a
@ja-tech{phrase}, @ja-tech{clause}, or @ja-tech{sentence}. Given that Japanese is an agglutinative
language with a rigid @ja-tech{SOV} topological structure, @ja-tech{cases} therefore play a prominent
role in marking grammatical structure as well as information structure and thematic role.

In the sense of the @ja-tech{case} category, the standard modern Japanese has been categorized as an
@ja-deftech["nominative-accusative language" 対格言語 たいかくげんご 宾格语言] in which case the @ja-tech{subject}
of the @ja-tech{transitive verb} and the @ja-tech{intransitive verb} share the same @ja-tech{case},
but are distinguished from the @ja-tech{case} of object of the @ja-tech{transitive verbs}.

Japanese implements its @ja-tech{case} system by postpositional @ja-tech{particles} which
more precisely are also known as @ja-tech{case particles}. These @ja-tech{case particles}
are phonologically bound to the preceding words as the form @ja-form{@ja-tech{Substantive}
 + @ja-tech{Case Particle}} is, nevertheless, @ja-tech{case particles} are actually phrasal
clitics rather than nominal declensions so that other elements (e.g. @ja-tech{adverbial particles})
may intervene between @ja-tech{case particles} and the marking @ja-tech{substantives} or
extend the scope over more than one @|NP|@ja-exref[interleaved-nps].

@ja-exemplify{interleaved-nps}

One of the key features of Japanese @ja-tech{case} system is based on its @ja-tech{topic}-prominent
nature. Multiple nominative-marked noun @ja-tech{phrase}s may occure within one clause so that the
initial one may be considered as being characterized by the remaining part of the clause@ja-exref[hallmark:multi-noms].
This phenomenon has become a major problem for describing Japanese @ja-tech{case} system.

@ja-exemplify{hallmark:multi-noms}

@handbook-scenario[#:tag "case"]{@ja-title[#:ja-term? #false "Standard Cases" "標準格" "ひょうじゅんかく" 标准格]}

@ja-tech{Case}s are language-specific entity@subscript{@~cite[ToC]} which means @ja-tech{case} labels are only
valid for particular languages. For convenience, similar @ja-tech{case} labels are used for different languages,
nonetheless, it is still senseless to compare @ja-tech{case}s that share same names among languages. Besides,
linguists with different backgrounds use same terms for different concepts, or use different terms for very
similar or identical concepts. This issue is even majorer in the @ja-tech{case} category.

As agglutinative language tends to have a bigger @ja-tech{case} system, there are 10 primary @ja-tech{case}s
marked by 10 @ja-tech{case particle}s in the standard modern Japanese.

@handbook-action[#:tag "NOM:ga"]{@ja-title[#:abbr NOM Nominative 主格 しゅかく 主格] 「~が」}

The @deftech{nominative} is usually the @ja-tech{subject} of a @ja-tech{verb} or a @ja-tech{predicate},
and be marked by the @ja-tech{case particle} が@ja-exref[nom:ga]. Despite the fact that は-marked
@ja-tech{topic} may also be interpreted as the @ja-tech{subject}, the mainstream suggests that Japanese
@ja-tech{subject} refers to the logical @ja-tech{subject} instead of the @ja-tech{topic}@ja-exref[nom:ga d].

@ja-exemplify{nom:ga}

What is triky here is that が-marked @ja-tech{nominative}s may have no corresponding grammatical function.
が marks @ja-tech{subject} arguments as well as non-@ja-tech{subject} arguments@ja-exref[nom:ga:nsub], especially
for multi @ja-tech{nominative}-marked @ja-tech{clause}s where the initial @ja-tech{nominative} is considered as the
@ja-tech{subject} but the rest may just be left undefined in the sense of grammar@ja-exref[hallmark:multi-noms].

@ja-exemplify{nom:ga:nsub}

In @ja-ExRef[nom:ga:nsub], those が-marked @ja-tech{nominative}s are typically considered as
@ja-deftech[targets 対象語 たいしょうご], which is a subtype of the @ja-tech{object} or @ja-tech{complement},
 usually being the target of emotion, wishes or ability.

@ja-exemplify{nom:ga:ambiguity}

Note the difference between @ja-ExRef[nom:ga d] and @ja-ExRef[nom:ga:ambiguity] both of which start with a
@ja-tech{topic}. The @ja-tech{topic} of the former is actually an @ja-tech{object} placed before the
@ja-tech{subject}, whereas the @ja-tech{topic} of the latter is the whole containing the part characterized
by its @ja-tech{predicate}. It's still not clear whether the @ja-tech{nominative} in @ja-ExRef[nom:ga:ambiguity]
should be considered as the @ja-tech{subject}.

More about @ja-tech{subject} and @ja-tech{topic} is discussed in @secref{grammar/topic.scrbl}.

@handbook-action[#:tag "ACC:o"]{@ja-title[#:abbr ACC Accusative 対格 たいかく 宾格] 「~を」}

The @deftech{accusative} is the direct @ja-tech{object} of a @ja-tech{transitive verb}.

@handbook-action[#:tag "GEN:no"]{@ja-title[#:abbr GEN Genitive 属格 ぞっかく 领格] 「~の」}

@handbook-action[#:tag "DAT:ni"]{@ja-title[#:abbr DAT Dative 与格 よかく 与格] 「~に」}

@handbook-action[#:tag "LOC:de/ni"]{@ja-title[#:abbr LOC Locative 処格 しょかく 场所格] 「~で」/「~に」}
@handbook-action[#:tag "ALL:e/ni"]{@ja-title[#:abbr ALL Allative 向格 こうかく 方向格] 「~へ」/「~に」}

@handbook-action[#:tag "INSTR:de"]{@ja-title[#:abbr INSTR Instrumental 具格 ごかく 工具格] 「~で」}
@handbook-action[#:tag "ABL:kara"]{@ja-title[#:abbr ABL Ablative 奪格 だっかく 夺格] 「~から」}
@handbook-action[#:tag "COM:to"]{@ja-title[#:abbr COM Comitative 共格 きょうかく 共格] 「~と」}
@handbook-action[#:tag "TERM:made"]{@ja-title[#:abbr TERM Terminative 到格 とうかく 到格] 「~まで」}
@handbook-action[#:tag "COMP:yori"]{@ja-title[#:abbr COMP Comparative 比較格 ひかくかく 比较格] 「~より」}

@handbook-scenario{@ja-title[Valency 結合価 けつごうか 配价]}

@handbook-reference[]
