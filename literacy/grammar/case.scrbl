#lang scribble/book

@require{../literacy.rkt}
@require{../token.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title[Case 格 かく 格]}

Generally speaking, A @deftech{case} is any of the forms of a @ja-tech{noun}, @ja-tech{adjective},
or @ja-tech{pronoun} that express the semantic relation of a word to other words, especially to
@ja-tech{verbs}, in a @ja-tech{phrase}, @ja-tech{clause}, or @ja-tech{sentence}. Given that
Japanese is an agglutinative language with a rigid @ja-tech{SOV} topological structure,
@ja-tech{cases} therefore play a prominent role in marking grammatical structure as well
as information structure and thematic role.

Japanese implements its @ja-tech{case} system by postpositional @ja-tech{particles} which
more precisely are also known as @ja-tech{case particles}. These @ja-tech{case particles}
are phonologically bound to the preceding words as the form @ja-form{@ja-tech{Substantive}
 + @ja-tech{Case Particle}} is, nevertheless, @ja-tech{case particles} are actually phrasal
clitics rather than nominal declensions so that other elements (e.g. @ja-tech{adverbial particles})
may intervene between @ja-tech{case particles} and the marking @ja-tech{substantives} or
extend the scope over more than one @|NP|s@ja-exref[interleaved-nps].

@ja-exemplify{interleaved-nps}

One of the key features of Japanese @ja-tech{case} system is based on its @ja-tech{topic}-prominent
nature@handbook-footnote{Korean and Mandarin Chinese are also topic-prominent languages}.
Multiple nominative-marked noun @ja-tech{phrase}s may occure within one clause so that the
initial one may be considered as the @ja-tech{topic} and described by the remaining part of the
clause@ja-exref[hallmark:multi-noms]. This phenomenon has become a major problem for describing
Japanese @ja-tech{case} system.

@ja-exemplify{hallmark:multi-noms}

@handbook-scenario[#:tag "case"]{@ja-title[#:ja-term? #false "Case and Sentence Component" "格|と|文|の|成分" "かく|-|ぶん|-|せいぶん" 格与句子成分]}

In the sense of @ja-tech{case}, the standard modern Japanese has been categorized as an
@ja-deftech["accusative language" 対格言語 たいかくげんご 宾格语言] in which case the @ja-tech{subject}
of the @ja-tech{transitive verb} and the @ja-tech{intransitive verb} share the same @ja-tech{case},
but are distinguished from the @ja-tech{case} of object of the @ja-tech{transitive verbs}.

There are 10 prominent @ja-tech{case particle}s and 10 prominent @ja-tech{case}s in the standard modern Japanese,
desipte the fact that they are not one-to-one corresponding.

@handbook-action[#:tag "ga:nominative"]{が @ja-title[#:abbr NOM Nominative 主格 しゅかく 主格]}

The @deftech{nominative} is used for marking the @ja-tech{subject} of a @ja-tech{verb} as well as a @ja-tech{predicate}.

@handbook-action[#:tag "o:accusative"]{を @ja-title[#:abbr ACC Accusative 対格 たいかく 宾格]}

The @deftech{accusative} is used for marking the direct @ja-tech{object} of a @ja-tech{transitive verb}.

@;{
(list  (list @ja-deftech[#:abbr DAT Dative 与格 よかく 与格] "〜に" 'cont)
       (list @ja-deftech[#:abbr INSTR Instrumental 具格 ごかく 工具格] "〜で" 'cont)
       (list @ja-deftech[#:abbr LOC Locative 処格 しょかく 场所格] "〜で、〜に" 'cont)
       (list @ja-deftech[#:abbr ALL Allative 向格 こうかく 方向格] "〜へ、〜に" 'cont)
       (list @ja-deftech[#:abbr ABL Ablative 奪格 だっかく 夺格] "〜から" 'cont)
       (list @ja-deftech[#:abbr GEN Genitive 属格 ぞっかく 领格] "〜の" 'cont)
       (list @ja-deftech[#:abbr COM Comitative 共格 きょうかく 共格] "〜と" 'cont)
       (list @ja-deftech[#:abbr TERM Terminative 到格 とうかく 到格] "〜まで" 'cont))]
}

@handbook-scenario{@ja-title[Valency 結合価 けつごうか 配价]}

@handbook-reference[]
