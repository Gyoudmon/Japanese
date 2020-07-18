#lang scribble/book

@require{../literacy.rkt}
@require{../token.rkt}

@(define-book-bib ToC "The Handbook of Case" "Martin Haspelmath" "Oxford University Press"
   #:chapter "Terminology of Case"
   #:date 2006
   #:url "https://www.eva.mpg.de/fileadmin/content_files/staff/haspelmt/pdf/CaseTerminology.pdf")

@(define-book-bib Hyphen "The Element of Style" (authors "William Strunk Jr." "E. B. White") "Allyn & Bacon"
   #:chapter "A Few Matters of Form"
   #:pages (list 41 42)
   #:date 1959)

@(define-journal-bib TCH "The Case Hierarchy" "Barry Blake" "La Trobe Working Papers in Linguistics"
   #:volume 5
   #:pages (list 1 6)
   #:date 1992
   #:url "https://web.archive.org/web/20070929161614/http://www.latrobe.edu.au/linguistics/LaTrobePapersinLinguistics/Vol%2005/01Blake.pdf")

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
but are distinguished from the @ja-tech{case} of @ja-tech{object} of the @ja-tech{transitive verbs}.

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

@handbook-scenario[#:tag "case"]{@ja-title[#:ja-term? #false "Surface Cases" "表層格" "ひょうそうかく" 表层格]}

@ja-tech{Case}s are language-specific entity@~subcite[ToC] which means @ja-tech{case} labels are only valid
for particular languages. For convenience, similar @ja-tech{case} labels are used for different languages,
nonetheless, it is still senseless to compare @ja-tech{case}s that share same names among languages. Besides,
linguists with different backgrounds use same terms for different concepts, or use different terms for very
similar or identical concepts. This issue is even majorer in the @ja-tech{case} category.

As agglutinative language tends to have a bigger @ja-tech{case} system, there are 11 primary @ja-tech{case}s
marked by 10 @ja-tech{case particle}s in the standard modern Japanese.

@handbook-action[#:tag "NOM:ga"]{@ja-title[#:abbr NOM Nominative 主格 しゅかく 主格] 「~が」}

The @deftech{nominative} usually functions as the @ja-tech{subject} of a @ja-tech{verb} or a @ja-tech{predicate},
and be marked by the @ja-tech{case particle} が@ja-exref[nom:ga]. Despite the fact that は-marked
@ja-tech{topic} may also be interpreted as the @ja-tech{subject}, the mainstream suggests that Japanese
@ja-tech{subject} refers to the logical @ja-tech{subject} instead of the @ja-tech{topic}@ja-exref[nom:ga d].

@ja-exemplify{nom:ga}

What is triky here is that が-marked @ja-tech{nominative}s may have no corresponding grammatical function.
が marks @ja-tech{subject} arguments as well as non-@ja-tech{subject} arguments@ja-exref[nom:ga:nsub], especially
for multiple @ja-tech{nominative}-marked @ja-tech{clause}s where the initial @ja-tech{nominative} is considered as
the @ja-tech{subject} but the rest may just be left undefined in the sense of grammar@ja-exref[hallmark:multi-noms].

@ja-exemplify{nom:ga:nsub}

Those @ja-tech{nominative}s in @ja-ExRef[nom:ga:nsub] are typically considered as the
@ja-deftech[target 対象語 たいしょうご], which is a subtype of the @ja-tech{object} or @ja-tech{complement},
usually being the target of emotion, feeling, wishes or ability.

@ja-exemplify{nom:ga:ambiguity}

Note the difference between @ja-ExRef[nom:ga d] and @ja-ExRef[nom:ga:ambiguity] both of which start with
a @ja-tech{topic}. The @ja-tech{topic} of the former actually acts as an @ja-tech{object} placed before the
@ja-tech{subject}, whereas the @ja-tech{topic} of the latter refers to the whole that containing the part
referred by the @ja-tech{nominative} and characterized by its @ja-tech{predicate}. It's still not clear whether
the @ja-tech{nominative} in @ja-ExRef[nom:ga:ambiguity] should be analyzed as the @ja-tech{subject}.

For more uses of @ja-tech{nominative}, see @secref{valence}.
For more about @ja-tech{subject} and @ja-tech{topic}, see @secref{category/topic.scrbl}.

@handbook-action[#:tag "ACC:o"]{@ja-title[#:abbr ACC Accusative 対格 たいかく 宾格] 「~を」}

The @deftech{accusative} functions as the direct @ja-tech{object} of a @ja-tech{transitive verb}@ja-exref[acc:o a],
or the @ja-tech{complement} of an @ja-tech{intransitive verb}, and be marked by the @ja-tech{case particle} を.

@ja-exemplify{acc:o}

More precisely, を-marked @ja-tech{complement}s identify the source@ja-exref[acc:o b] or path@ja-exref[acc:o c] arguments
of motion @ja-tech{verb}s. For path arguments, they can only be marked by the @ja-tech{accusative} particle; while for
source arguments, they can also be marked by the @ja-tech{ablative} particle, but note that not all source arguments can
be marked by the @ja-tech{accusative} particle@ja-exref[acc:o:inanimate]. That is, @ja-tech{accusative}s only work with
animate @ja-tech{subject}s that are volitional.

@ja-exemplify{acc:o:inanimate}

@handbook-action[#:tag "GEN:no"]{@ja-title[#:abbr GEN Genitive 属格 ぞっかく 领格] 「~の」}

The @deftech{genitive} relates the possessor @ja-tech{substantive} to the head @ja-tech{substantive}, or indicates the
appositive of another @ja-tech{substantive} that sharing the same referent, and typically be marked by the
@ja-tech{particle} の@handbook-footnote{It has been a standard practice to treat の as a @ja-tech{case particle}.
 @ruby["並立助詞" "へいりつじょし"] or @ruby["連体助詞" "れんたいじょし"] might be more appropriate.}.

The possession has a much broader interpretation than ownership. Property, material, quantity, time, place, @|etc|
All these asymmetric relationships are counted on. So that のs may not appear exactly in the translated sentences.

@ja-exemplify{gen:no}

@ja-tech{Genitives} are the source of ambiguities when they are linked together@ja-exref[gen:no:ambiguity] as the
form @ja-form{SubstantiveのSubstantive} also identifies a @ja-tech{substantive}. Usually the @ja-tech{genitive}
chain is interpreted from left to right, but this convention seems not to be guaranteed by any rules.

@ja-exemplify{gen:no:ambiguity}

Nonetheless, the appositive is only allowed to be followed by a @ja-tech{substantive} that does not contain another
@ja-tech{genitive}@ja-exref[gen:no:apposition a], and to do a exhaustive enumerating, the sentence has to be constructed
in another way@ja-exref[gen:no:apposition b].

@ja-exemplify{gen:no:apposition}

Revisiting @ja-ExRef[nom:ga:ambiguity]. What has already been learnt is that the @ja-tech{topic} refers to the whole
that containing the part referred by the @ja-tech{nominative}, it is a good chance to transform that @ja-tech{complex sentence},
whose @ja-tech{predicate} is also a @ja-tech{subject}-@ja-tech{predicate} @ja-tech{clause}, into a
@ja-tech[#:key "simple sentence"]{simple one} with @ja-tech{genitive}@ja-exref[gen:no:transform].

@ja-exemplify{gen:no:transform}

Historically speaking, the @ja-tech{particle} の is a marker of adnominal modification, and turns the word or
@ja-tech{phrase} it attached into an @ja-tech{attributive modifier}@ja-exref[gen:no:attributive]. But it remains
to be seen whether the use of の as possessive marker should be analyzed as a subtype of adnominal modification marker.

@ja-exemplify{gen:no:attributive}

@ja-tech{Case} instances are regualr seen as the postpositional @ja-tech{phrase} attached by の. What irregular here
is that there are no such formatives like @ja-form{がの}, @ja-form{をの}, and @ja-form{にの}, where the first two turn
into just @ja-form{の}s and the third one turns into @ja-form{への}@ja-exref[gen:no:attributive c].

Further more, の may also function as a nominalizer@ja-exref[gen:no:nominalizer a] or a pro-form that refers back
to an entity that is recoverable from the discourse context@ja-exref[gen:no:nominalizer b]. In these situations,
the の and the marking word or phrase as a whole becomes a @ja-tech{substantive}, and no other @ja-tech{substantive}
is following it.

@ja-exemplify{gen:no:nominalizer}

Anyway, in both English and Mandarin Chinese, a @ja-tech{noun} can be modified by another @ja-tech{noun} directly,
whereas Japanese does so with @ja-tech{genitive}s. As mentioned in @~cite[Hyphen], two words tend to be combined
as a union, which eventually becomes a new word, usually after a period of hyphenation. To illustrate: @tt{wild life}
⇒ @tt{wild-life} ⇒ @tt{wildlife}. It is reasonable to consider Japanese compound @ja-tech{noun}s the same way, as
this pattern has already been seen not rarely, such as @ja-word["wild animal" 野生動物 やせいどうぶつ],
@ja-word["math book" 数学本 すうがくほん]. As expected, some cases may collide with existing words,
or change meanings and/or spells, including @ja-word["girl" "女|の|子" "おんな|-|こ"] and
@ja-word["woman/girl" "女子" "じょし"].

@handbook-action[#:tag "DAT:ni"]{@ja-title[#:abbr DAT Dative 与格 よかく 与格] 「~に」}

The @deftech{dative} usually functions as the indirect @ja-tech{object} of a @ja-tech{ditransitive verb}, and be marked
by the @ja-tech{case particle} に@handbook-footnote{From now on, things become disorder as uses of @ja-tech{dative}
 overlap uses of many other @ja-tech{case particle}s, it is reasonable to consider this phenomenon from the perspective
 of the @ja-tech{case} hierarchy introduced in @~cite[TCH]. For short, @ja-tech{dative} is the last core @ja-tech{case}
 after @ja-tech{nominative}, @ja-tech{accusative}, and @ja-tech{genitive}, later on more @ja-tech{case}s are defined
 to make expressions more detailed.}. The word @ja-tech{dative} itself is derived from the Latin @ja-quote{(case) of giving},
which semantics can be phrased as @ja-form{give B to A}@ja-exref[dat:ni:give a] or @ja-form{make (B) for A}@ja-exref[dat:ni:give b],
where @ja-form{A} is the indirect @ja-tech{object}, someone or something, should be marked by に.

@ja-exemplify{dat:ni:give}

For pattern A as exemplified in @ja-ExRef[dat:ni:give a], the indirect @ja-tech{object} follows the preposition
@ja-form{to}, which also be used to identify the destination of motion @ja-tech{verb}s or purpose coincidentally.
Thus, in some languages influenced by English, directly or indirectly, use of @ja-tech{dative} overlaps use of
@ja-tech{allative}@ja-exref[dat:ni:all].

@ja-exemplify{dat:ni:all}

When the purpose argument is provided, it must precede the motion @ja-tech{verb} immediately, and its に-marking word is
@ja-tech{the continuative form} of a @ja-tech{verb}@ja-exref[dat:ni:all b] or a @ja-tech{verbal noun}@ja-exref[dat:ni:all c].
Note that @ja-tech{the continuative form} of the verbalized @ja-tech{verbal noun} should be し-suffixed, but
if so the sentence would sound like an oriental curse: @ja-word["go to die" "死|に|行|く" "し|-|い|"]. Thus,
@ja-tech{verbal noun}s tend to be used directly instead, though the し-suffixed versions work as well.

For pattern B as exemplified in @ja-ExRef[dat:ni:give b], it can be semantically extended to express the existence
of @ja-form{B} in place @ja-form{A}. This use@ja-exref[dat:ni:loc] overlaps the use of @ja-tech{locative}.

@ja-exemplify{dat:ni:loc}

Literally translating, the resulting @ja-tech{sentence} would be the second one, which is actually ambiguity and
yet indistinguishable from @ja-ExRef[loc:de]. What really should be concerned here is the existence of the entity
eventually made by the action rather than the action itself producing the entity, so both the first and third ones
are accurate despite the verboseness of the third one.

Particularly, there are two shortcuts to express existence, one is いる for animate entities@ja-exref[dat:ni:exist a],
the other is ある for ianimate entities@ja-exref[dat:ni:exist b].

@ja-exemplify{dat:ni:exist}

By far, the @ja-tech{dative} has been adapted to a wide range of uses in addition to those two basic patterns.
Nonetheless, it is reasonable to only mention distinct uses here@ja-exref[dat:ni], and some reorganization
might be performed.

@ja-exemplify{dat:ni}

For special uses of @ja-tech{dative}, see @secref{valence}.

@handbook-action[#:tag "LOC:de/ni"]{@ja-title[#:abbr LOC Locative 処格 しょかく 场所格] 「~で」「~に」}

The @deftech{locative} specifies a location where events or actions take place, and be marked by the
@ja-tech{case particle} で or に. Be careful, the term @ja-tech{locative} is a little tricky. As
@deftech{spatial relation}s can be grouped into 4 directional classes: location @ja-word{at}, goal
@ja-word{to, toward}, source @ja-word{from}, and path @ja-word{along, through}. In a broad sense,
each of these four classes is associated with a place as its location, but the @ja-tech{locative}
identified location only stands for the first class.

So that the overlap among uses of @ja-tech{dative} and @ja-tech{locative} is all around the semantics of
existence. に indicates the location where someone or something exists in which case the way to arrive or
to be made is trivial@ja-exref[dat:ni:loc], whereas で indicates the location that some events or actions
occur in which case the side effects of the events or actions are trivail@ja-exref[loc:de].

@ja-exemplify{loc:de}

Despite the fact that @ja-tech{locative} is actually where で started as a @ja-tech{case particle}, now
で has been considered as the @ja-tech{instrumental}, and the location has been categorized as an instrument.
Based on this fact, で-marked @ja-tech{locative} can be better explained: the location as an instrument
provides some particular functions that benefit the event or action, independent of the goal, and eventually
be chosen by the agents.

Take it easy, and don't hang on these explainations. で and に as the @ja-tech{locative} marker are virtually
interchangeable since additional information can be inferred from contexts.

By the way, some @ja-tech{substantive}s marked by で may look like locations, but they actually represent
ranges or collections@ja-exref[instr:de:range].

@ja-exemplify{instr:de:range}

@handbook-action[#:tag "ALL:e"]{@ja-title[#:abbr ALL Allative 向格 こうかく 方向格] 「~へ」「~に」}

The @deftech{allative} specifies the goal of a motion @ja-tech{verb} and stands for the second class of the
@ja-tech{spatial relation}, and typically be marked by the @ja-tech{case particle} へ or に@ja-exref[all:e].

@ja-exemplify{all:e}

Similar to で and に as the @ja-tech{locative} marker, へ and に as the @ja-tech{allative} marker are also
virtually interchangeable. へ tends to be used when the direction is more emphasized, whereas に pays more
attention to the destination or purpose@ja-exref[all:e b].

@ja-exemplify{all:e:standalone}

Besides, に-marked @ja-tech{allative} can stand alone without an explicit action@ja-exref[all:e:standalone],
especially when combined with の to form an adnominal modifier@ja-exref[gen:no:attributive c].

@handbook-action[#:tag "INSTR:de"]{@ja-title[#:abbr INSTR Instrumental 具格 ごかく 工具格] 「~で」}

@deftech{instrumental}

@handbook-action[#:tag "ABL:kara"]{@ja-title[#:abbr ABL Ablative 奪格 だっかく 夺格] 「~から」}

@deftech{ablative}

@handbook-action[#:tag "TERM:made"]{@ja-title[#:abbr TERM Terminative 到格 とうかく 到格] 「~まで」}

@deftech{terminative}

@handbook-action[#:tag "COM:to"]{@ja-title[#:abbr COM Comitative 共格 きょうかく 共格] 「~と」}

The @deftech{comitative} encodes the relationship of accompaniment between two participants in an event,
and be marked by the @ja-tech{case particle} と@ja-exref[com:to]. Both of the participants are agents of
the action, this is the key point for distinguishing between the @ja-tech{comitative} and the
@ja-tech{instrumental}.

@ja-exemplify{com:to}

The @ja-tech{adverb} 一緒に is often used together if one of the participants is already adequate to the
event or action@ja-exref[com:to a].

@ja-exemplify{com:to:and}

Like の, と can also be used as a @ja-tech{parallel particle} to combine @ja-tech{substantive}s, in which
case the compound @ja-tech{substantive} does not introduce new @ja-tech{sentence} component@ja-exref[com:to:and a],
whereas と-marked @ja-tech{comitative} functions as the @ja-tech{complement}@ja-exref[com:to b] of the action.
Nonetheless, such a subtle difference usually does not make sense.

と as a @ja-tech{parallel particle} implies the exhaustive emueration. That is, all @ja-tech{substantive}s
in the mentioned collection must be combined one by one explicitly@ja-exref[com:to:enum a]. For non-exhaustive
enumeration, alternatives are available, such as combining the @ja-tech{parallel particle} や @ja-word{and, or}
and the @ja-tech{adverbial particle} など @ja-word{@etc}@ja-exref[com:to:enum b].

@ja-exemplify{com:to:enum}

Note that や and など combination is not a set collocation. That is, both of them can be used without
the other as long as their literal meanings are appropreciate and not redundant.

Another alternative is the @ja-tech{parallel particle} とか which sounds more colloquial than や. とか
should mark all enumerated items including the last one@ja-exref[com:to:enum:toka].

@ja-exemplify{com:to:enum:toka}

As one can see, the enumerated items are not restricted to @ja-tech{substantive}s, the stem-form
@ja-tech{verb}s, @ja-tech{adjective}s, and @ja-tech{quotation}s are also acceptable.

For more uses of と, see @secref{semantics/quotation.scrbl}.

@handbook-action[#:tag "CMP:yori"]{@ja-title[#:abbr CMP Comparative 比較格 ひかくかく 比较格] 「~より」}

The @deftech{comparative} specifies the standard of comparison of inequality, and be marked by the
@ja-tech{case particle} より@ja-exref[cmp:yori]. Since the @ja-tech{comparative} can be considered
as a special case of @ja-tech{ablative}, and より do have some overlapping uses with から, some grammer
books may merge these two @ja-tech{case}s. 

@ja-exemplify{cmp:yori}

Both similative and superlative are unavailable in Japanese, nonetheless, these two semantics can be
expressed easily with or without @ja-tech{comparative}@ja-exref{cmp:yori:similative+superlative}.

@ja-exemplify{cmp:yori:similative+superlative}

@handbook-scenario[#:tag "valence"]{@ja-title[Valence　結合価 けつごうか 配价]}

@handbook-reference[]
