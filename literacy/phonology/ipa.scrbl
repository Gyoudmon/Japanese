#lang scribble/book

@require{../literacy.rkt}
@require{../chart.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib CardinalVowels "Cardinal Vowels" "https://en.wikipedia.org/wiki/Cardinal_vowels"
   #:author "Daniel Jones"
   #:date 1967)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define broad-style (make-style #false (list (make-color-property "DodgerBlue"))))
@(define narrow-style (make-style #false (list (make-color-property "SandyBrown"))))
@(define index-style (make-style #false (list (make-color-property "DimGray"))))

@(define vnode
   (lambda [unrounded [rounded #false] [name #false] #:ja-vowel [vowel 'none] #:narrow? [narrow? #false] #:dot [dot #true] #:elem [ipa-elem phone-elem]]
     (define uv-elem
       (and unrounded
            (elem #:style (and (memq vowel '(both left)) (if (not narrow?) broad-style narrow-style))
                  (ipa-elem (~a unrounded)))))
     
     (define rv-elem
       (and rounded
            (elem #:style (and (memq vowel '(both right)) (if (not narrow?) broad-style narrow-style))
                  (ipa-elem (~a rounded)))))

     (define notation
       (filter values
               (cond [(not dot) (list uv-elem rv-elem)]
                     [(not (pair? dot)) (list uv-elem full-space rv-elem)]
                     [else (let ([lidx (car dot)]
                                 [ridx (cdr dot)])
                             (list (list (subscript (elem #:style index-style (if (< lidx 10) (~a #\space lidx) (~a lidx)))) uv-elem)
                                   full-space
                                   (list rv-elem (subscript (elem #:style index-style (if (< ridx 10) (~a ridx #\space) (~a ridx)))))))])))
     
     (list (~a (or name (apply ~a (filter values (list unrounded rounded)))))
           notation
           (and dot #true))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{@ja-title[#:key "IPA" "International Phonetic Alphabet" 国際音標文字 こくさいおんぴょうもじ 国际音标]}

The @deftech[#:key "IPA"]{International Phonetic Alphabet(IPA)} is an alphabetic system of phonetic
notation based primarily on the Latin alphabet. It was devised the International Phonetic Association
(also abbreviated to IPA) and has become the standardized representation of @ja-tech{speech sound}s
for all (spoken) languages since 19th century.

The @ja-tech{IPA} provides notations for both @ja-tech{phone} and @ja-tech{phoneme}. By convention,
@ja-tech{phone}s and @ja-tech{phoneme}s are represented in @ipa-phone{ } and @ipa-phoneme{ }, respectively.

@handbook-scenario[#:tag "CardinalVowels"]{@ja-title[Vowels 母音 ぼいん 元音]}

@deftech{Vowel}s are produced with an open vocal tract, which means the airflow will not be disturbed
from vocal cords to lips to outside, and vary in quality, loudness, and length.

@tamer-figure["ipa:vowels" "IPA Vowels"]{
 @(let*-values ([(width height) (values 10 6)]
                [(xunit) (* width 1/6)]
                [(dx) (values (* xunit 2))])
    @nested[#:style "tikzpicture"]{
  @(ipa-line dx width height 'Open
             (vnode 'a '&OE 'aoe #:ja-vowel 'left #:dot (cons 4 12))
             (vnode '&ipacentralized.a #false 'ca #:ja-vowel 'left #:narrow? #true)
             (vnode 'A '6 #:dot (cons 5 13)))
  @(ipa-line dx width height 'Near-Open #:draw-line? #false
             (vnode '&ae #false 'ae)
             (vnode '5 #false #:dot #false))
  @(ipa-line dx width height 'Open-Mid
             (vnode 'E '&oe 'eoe #:dot (cons 3 11))
             (vnode '3 '&textcloserevepsilon '3cr3)
             (vnode '2 'O #:dot (cons 14 6)))
  @(ipa-line dx width height 'Mid #:draw-line? #false
             (vnode '&textlowering.e '&textlowering.&o 'cmo #:ja-vowel 'left #:narrow? #true)
             (vnode '* #false 'schwa #:dot #false)
             (vnode #false '&textlowering.o 'mbo #:ja-vowel 'right #:narrow? #true))
  @(ipa-line dx width height 'Close-Mid
             (vnode 'e '&o 'ecmo #:ja-vowel 'left #:dot (cons 2 10))
             (vnode '9 '8)
             (vnode '7 'o #:ja-vowel 'right #:dot (cons 15 7)))
  
  @(ipanode width height 'Near-Front 'Near-Close dx (vnode 'I 'Y))
  @(ipanode width height 'Near-Back 'Near-Close dx (vnode #false 'U))
  
  @(ipanode width height 'Near-Back 'Close dx (vnode '&textsubplus.W 'W+B 'nbW #:ja-vowel 'both #:narrow? #true))
  
  @(ipa-line dx width height 'Close
             (vnode 'i 'y #:ja-vowel 'left #:dot (cons 1 9))
             (vnode '1 '0 #:dot (cons 17 18))
             (vnode 'W 'u #:ja-vowel 'left #:dot (cons 16 8)))
  
  @(ipa-edge 'aoe 'ae 'eoe 'cmo 'ecmo 'iy)
  @(ipa-edge 'ca '5 '3cr3 'schwa '98 '10)
  @(ipa-edge 'A6 '2O 'mbo '7o 'Wu)
  
  @(for/list ([hlabel (in-list ipa-vowel-simplified-backnesses)])
     (ipanode width height hlabel 'Close dx 1 (vnode hlabel #:dot #false #:elem tt)))
  
  @(for/list ([vlabel (in-list ipa-vowel-heights)])
     (define-values (x y) (ipa-vowel-position width height 'Front vlabel dx))
     (ipanode 0 y (vnode vlabel #:dot #false #:elem tt)))
  })
}

@Tamer-Figure-ref{ipa:vowels} is the well known vowel chart defined in the @ja-tech{IPA}, extended
the @~cite[CardinalVowels], a measuring system of vowels that well trained phoneticians can produce
and recognize. That is, vowels defined in @tamer-figure-ref{ipa:vowels} are just abstract references,
instead of exact mappings, of real vowels in a particular language, although for some languages, some
of their vowels may happen to coincide with the @ja-tech{cardinal vowel}s. Nonetheless,
@tamer-figure-ref{ipa:vowels} still deserves its place for beginners due to its intuitiveness.

A @ja-deftech["cardinal vowel" 基本母音 きほんぼいん 定位元音] is defined to be produced when the tongue
is in an extreme position, either front or back, high or low@handbook-footnote{The position involves
 two dimensions, named @emph{vowel height} and @emph{vowel backness}, which are relative to the roof
 and back of the mouth, respectively. By a formal definition, these two features refer to the first
 and second formant of the vocal spectrogram, but the two formants consistently associated with the
 position of the tongue and jaw, thus, it is safe to follow the simple definition. Given that opening
 the mouth will lower the tongue from the viewpoint of the palate, moving the tongue vertically can
 be simplified to opening and closing the mouth, which adjusts the aperture of the jaw. As is in
 @tamer-figure-ref{ipa:vowels}, the @ja-tech{IPA} prefers the @tt{open}/@tt{close} model, which inverts
 the @tt{high}/@tt{low} model.}. The three corner vowels, @ipa-phone{i}, @ipa-phone{A}, and @ipa-phone{u},
have articulatory definitions, other cardinal vowels are auditorily equidistant amongst them. More
precisely, @ipa-phone{i} is produced with the tongue as far forward and as high in the mouth as possible
without producing friction; @ipa-phone{u} is produced with the tongue as far back and as high in the
mouth as possible with protruded lips, similar to blowing out a candle; and @ipa-phone{A} is produced
with the tongue as low and as far back in the mouth as possible. Along with four vowels at trisection
points, @ipa-phone{e}, @ipa-phone{E}, @ipa-phone{O}, and @ipa-phone{o}, plus the @tt{front-open} vowel
@ipa-phone{a}, these eight vowels are common in natural languages, and therefore be categorized as
the @ja-deftech["primary cardinal vowel" 第一次基本母音 だいいちじきほんぼいん 主定位元音], the rest,
whose numerical labels range from 9 to 18, are categorized as the
@ja-deftech["secondary cardinal vowel" 第二次基本母音 だいにじきほんぼいん 次定位元音].

For each pair of vowels at the same position, they differ in roundedness of lips. By some phonetic
correlation between rounding and backness@handbook-footnote{Acoustical speaking, rounding tends to be
 make the second formant decrease.}, the unrounded vowels are placed at the left side, while the rounded
ones are placed at the right side. The two counterexamples, @ipa-phone{*} and @ipa-phone{5}, do not have
definitions in sense of the roundedness, and are more often unrounded than rounded.

Generally speaking, there are only 5 @ja-tech{vowel}s in Japanese. In a broad sense, these @ja-tech{vowel}s
can be simply transcribed as the five blue symbols in @Tamer-Figure-ref{ipa:vowels}, while in a narrow
sense, if compared to similar @ja-tech{vowel}s in other languages, they should be transcribed as the
orange ones@handbook-footnote{Thay are marked with diacritics whose meanings can be observed in the
 chart directly. @ipa-phone{&textlowering.◌$} means @emph{lowered}, @ipa-phone{&textsubplus.◌$} means
 @emph{advanced}, and @ipa-phone{&ipacentralized.◌$} means @emph{centralized}.}(plus the blue @ipa-phone{i}).
In other words, the orange symbols represent the true @ja-tech{vowel}s of Japanese, but the blue ones
are also acceptable for easy transcribing@handbook-footnote{@ipa-phone{W} can be transcribed as @ipa-phone{u}
 for same reason.} because there is no phonemic distinction between the orange symbol and the blue
counterpart within Japanese.

In contrast to English and Mandarin Chinese, the extreme positions of Japanese vowels, benchmarked with
the @tt{open central unrounded vowel} @ipa-phone{&ipacentralized.a}, the @tt{close front unrounded vowel}
@ipa-phone{i}, and the @tt{close near-back vowel} @ipa-phone{&textsubplus.W$}, are actually less extreme
in both height and width dimension, which measures the other position of lips for an unrounded
@ja-tech{vowel}@handbook-footnote{The vowel chart gives no information about the ``vowel width''.
 For short, the extremely unrounded vowel is categorized as @emph{spread}, like smiling.}. The @ipa-phone{W}
is the most notable one among the five due to its special roundedness: it can be either unrounded(@ipa-phone{&textsubplus.W$})
or compressed(@ipa-phone{W+B}), which is the other type of roundedness in addition to the protrusion@handbook-footnote{The
 @ja-tech{IPA} does not define specialized diacritics for the two types of roundedness, the superscript
 letter @ipa-sym{+B} can be used for compression, and the @ipa-sym{+w}, usually omitted, for protrusion.}.

Last but most important, @emph{never} learn pronunciation from its written description, unless you have
already mastered the pronunciation of at least one foreign language with a rich set of phonetic features
by learning from professional teachers.

@handbook-scenario{@ja-title[Consonants 子音 しいん 辅音]}

@deftech{Consonant}s are produced with a partial or total closure of the vocal tract.

@handbook-reference[]
