#lang scribble/book

@require{../literacy.rkt}
@require{../ipa.rkt}

@require{../../digitama/ipa.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib CardinalVowels "Cardinal Vowels" "https://en.wikipedia.org/wiki/Cardinal_vowels"
   #:author "Daniel Jones"
   #:date 1967)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define full-space (string (integer->char #x3000)))
@(define broad-style (make-style #false (list (make-color-property "DodgerBlue"))))
@(define narrow-style (make-style #false (list (make-color-property "SandyBrown"))))

@(define phone-elem
   (lambda [phones]
     (larger (larger (ipa-sym phones)))))

@(define vnode
   (lambda [unrounded [rounded #false] [name #false] #:ja-vowel [ja-vowel 'none] #:narrow? [narrow? #false] #:dot? [dot? #true] #:elem [ipa-elem phone-elem]]
     (define uv-elem (and unrounded (elem #:style (and (memq ja-vowel '(both left)) (if (not narrow?) broad-style narrow-style)) (ipa-elem (~a unrounded)))))
     (define rv-elem (and rounded (elem #:style (and (memq ja-vowel '(both right)) (if (not narrow?) broad-style narrow-style)) (ipa-elem (~a rounded)))))
     (define notation (filter values (if (not dot?) (list uv-elem rv-elem) (list uv-elem full-space rv-elem))))
     (list (~a (or name (apply ~a (filter values (list unrounded rounded)))))
           notation
           dot?)))

@(define ipanode
   (case-lambda
     [(x y node)
      (and (list? node)
           (let ([path-node (multiarg-element "ipanode"
                                              (list (number->string x) (number->string y)
                                                    (car node) (cadr node)))])
             (cond [(not (caddr node)) (list path-node ";")]
                   [else (list path-node ";" (elem #:style "ipadot" (car node)) ";")])))]
     [(width height vowel-backness vowel-height dx node)
      (ipanode width height vowel-backness vowel-height dx 0 node)]
     [(width height vowel-backness vowel-height dx dy node)
      (define-values (x y) (ipa-vowel-position width height vowel-backness vowel-height dx dy))
      (ipanode x y node)]))

@(define ipa-edge
   (lambda names
     (cond [(null? names) null]
           [else (let draw ([head (car names)]
                            [rest (cdr names)]
                            [sward null])
                   (cond [(null? rest) (reverse sward)]
                         [else (draw (car rest) (cdr rest)
                                     (list* ";"
                                            (multiarg-element "ipaedge" (map ~a (list head (car rest))))
                                            sward))]))])))

@(define ipa-line
   (lambda [offset width height vowel-height vn1 vn2 [vn3 #false] #:draw-line? [draw-line? #true] #:dy [dy 0]]
     (define-values (xs y) (ipa-vowel-positions width height vowel-height offset dy))
     
     (filter values
             (list (ipanode (car xs) y vn1)
                   (ipanode (cadr xs) y vn2)
                   (ipanode (caddr xs) y vn3)
                   
                   (and draw-line?
                        (apply ipa-edge
                               (map car (filter values
                                                (list vn1 vn2 vn3)))))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{@ja-title[#:key "IPA" "International Phonetic Alphabet" 国際音標文字 こくさいおんぴょうもじ 国际音标]}

The @deftech[#:key "IPA"]{International Phonetic Alphabet(IPA)} is an alphabetic system of phonetic
notation based primarily on the Latin alphabet. It was devised the International Phonetic Association
(also abbreviated to IPA) and has become the standardized representation of @ja-tech{speech sound}s
for all (spoken) languages since 19th century.

The @ja-tech{IPA} provides notations for both @ja-tech{phone} and @ja-tech{phoneme}. By convention,
@ja-tech{phone}s and @ja-tech{phoneme}s are represented in @IPA{[ ]} and @IPA{/ /}, respectively.

@handbook-scenario{@ja-title[Vowels 母音 ぼいん 元音]}

@deftech{Vowel}s are produced with an open vocal tract, which means the airflow will not be disturbed
from vocal cords to lips to outside, and vary in quality, loudness, and length depending on the
roundedness of lips and position of the tongue root.

@tamer-figure["ipa:vowels" "IPA Vowels"]{
 @(let*-values ([(width height) (values 10 6)]
                [(xunit) (* width 1/6)]
                [(dx) (values (* xunit 2))])
    @nested[#:style "tikzpicture"]{
  @(ipa-line dx width height 'Open
             (vnode 'a '&OE 'aoe #:ja-vowel 'left)
             (vnode '&ipacentrialized.a #false 'ca #:ja-vowel 'left #:narrow? #true)
             (vnode 'A '6))
  @(ipa-line dx width height 'Near-Open #:draw-line? #false
             (vnode '&ae #false 'ae)
             (vnode '5 #false #:dot? #false))
  @(ipa-line dx width height 'Open-Mid
             (vnode 'E '&oe 'eoe)
             (vnode '3 '&textcloserevepsilon '3cr3)
             (vnode '2 'O))
  @(ipa-line dx width height 'Mid #:draw-line? #false
             (vnode '&textlowering.e '&textlowering.&o 'cmo #:ja-vowel 'left #:narrow? #true)
             (vnode '* #false 'schwa #:dot? #false)
             (vnode #false '&textlowering.o 'mbo #:ja-vowel 'right #:narrow? #true))
  @(ipa-line dx width height 'Close-Mid
             (vnode 'e '&o 'ecmo #:ja-vowel 'left)
             (vnode '9 '8)
             (vnode '7 'o #:ja-vowel 'right))
  
  @(ipanode width height 'Near-Front 'Near-Close dx (vnode 'I 'Y))
  @(ipanode width height 'Near-Back 'Near-Close dx (vnode #false 'U))
  
  @(ipanode width height 'Near-Back 'Close dx (vnode '&textsubplus.W #false 'nbW #:ja-vowel 'left #:narrow? #true))
  
  @(ipa-line dx width height 'Close
             (vnode 'i 'y #:ja-vowel 'left)
             (vnode '1 '0)
             (vnode 'W 'u #:ja-vowel 'left))
  
  @(ipa-edge 'aoe 'ae 'eoe 'cmo 'ecmo 'iy)
  @(ipa-edge 'ca '5 '3cr3 'schwa '98 '10)
  @(ipa-edge 'A6 '2O 'mbo '7o 'Wu)
  
  @(for/list ([hlabel (in-list ipa-vowel-simplified-backnesses)])
     (ipanode width height hlabel 'Close dx 1 (vnode hlabel #:dot? #false #:elem tt)))
  
  @(for/list ([vlabel (in-list ipa-vowel-heights)])
     (define-values (x y) (ipa-vowel-position width height 'Front vlabel dx))
     (ipanode 0 y (vnode vlabel #:dot? #false #:elem tt)))
  })
}

@Tamer-Figure-ref{ipa:vowels} is the well known vowel chart defined by @ja-tech{IPA}, which similar to
@~cite[CardinalVowels], a measuring system of vowels that well trained phoneticians can produce and
recognize. That is, vowels defined in @tamer-figure-ref{ipa:vowels} are just abstract references, instead
of exactly mappings, of real vowels, although for some languages, some of their vowels may happen to
coincide with the cardinal vowels. Nonetheless, @tamer-figure-ref{ipa:vowels} still deserves its place
for beginners due to the intuitiveness.

A @ja-deftech["cardinal vowel" 基本母音 きほんぼいん 定位元音] is defined to be produced when the tongue
is in an extreme position, either front or back, high or low. The three corner vowels @ipa-/sym/[#:phonetical? #true]{i},
@ipa-/sym/[#:phonetical? #true]{A}, and @ipa-/sym/[#:phonetical? #true]{u} have articulatory
definitions, other vowels@handbook-footnote{Note that IPA defines more vowels than cardinal vowels.
 In this paragraph, we are talking about cardinal vowels.} are auditorily equidistant amongst them.
More precisely, @ipa-/sym/[#:phonetical? #true]{i} is produced with the tongue as far forward and as
high in the mouth as possible without producing friction; @ipa-/sym/[#:phonetical? #true]{u} is produced
with the tongue as far back and as high in the mouth as possible with protruded lips, like blowing
out a candle; and @ipa-/sym/[#:phonetical? #true]{A} is produced with the tongue as low and as far
back in the mouth as possible. With four vowels at quarters @ipa-/sym/[#:phonetical? #true]{e},
@ipa-/sym/[#:phonetical? #true]{E}, @ipa-/sym/[#:phonetical? #true]{O}, and @ipa-/sym/[#:phonetical? #true]{o},
plus the front-open vowel @ipa-/sym/[#:phonetical? #true]{a}, these eight vowels are common in the
natural languages, and therefore be considered as @ja-deftech["primary cardinal vowel" 第一次基本母音 だいいちじきほんぼいん 主定位元音],
the rest are @ja-deftech["primary cardinal vowel" 第二次基本母音 だいにじきほんぼいん 次定位元音].

@handbook-scenario{@ja-title[Consonants 子音 しいん 辅音]}

@deftech{Consonant}s are produced with a partial or total closure of the vocal tract.

@handbook-reference[]
