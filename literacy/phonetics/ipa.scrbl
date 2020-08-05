#lang scribble/book

@require{../literacy.rkt}
@require{../ipa.rkt}

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
   (lambda [x y node]
     (and (list? node)
          (let ([path-node (multiarg-element "ipanode"
                                             (list (number->string x) (number->string y)
                                                   (car node) (cadr node)))])
            (cond [(not (caddr node)) (list path-node ";")]
                  [else (list path-node ";" (elem #:style "ipadot" (car node)) ";")])))))

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

@(define ipa-x
   (lambda [offset fx0 dx y]
     (+ (- fx0 (* dx y)) offset)))

@(define ipa-line
   (lambda [offset fx0 dx xn0 y vn1 vn2 [vn3 #false] #:draw-line? [draw-line? #true]]
     (define x0 (ipa-x offset fx0 dx y))
     (define xn (+ xn0 offset))
     (define xm (+ x0 (* (- xn x0) 1/2)))
     
     (filter values
             (list (ipanode x0 y vn1)
                   (ipanode xm y vn2)
                   (ipanode xn y vn3)
                   
                   (and draw-line?
                        (apply ipa-edge
                               (map car (filter values
                                                (list vn1 vn2 vn3)))))))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title["International Phonetic Alphabet" 国際音標文字 こくさいおんぴょうもじ 国际音标]}

The @deftech[#:key "IPA"]{IPA} is an alphabetic system of phonetic notation based primarily on the
Latin alphabet. It has become the standardized representation of @ja-tech{speech sound}s for all
(spoken) languages since 19th century.

The @ja-tech{IPA} provides notations for both @ja-tech{phone} and @ja-tech{phoneme}. By convention,
@ja-tech{phone}s and @ja-tech{phoneme}s are represented in @IPA{[ ]} and @IPA{/ /}, respectively.

@handbook-scenario{@ja-title[Vowels 母音 ぼいん 元音]}

@deftech{Vowel}s are produced with an open vocal tract, which means the airflow will not be disturbed
from vocal cords to lips, and vary in quality, loudness, and length depend on the shape or positions
of the tongue, teeth and lips.

@handbook-figure["ipa:vowels" "IPA: Vowels"]{
 @(let* ([width 10]
         [xunit (* width 1/15)]
         [fx0 (- (* width 1/2) (* xunit 1/10))]
         [dx (* fx0 1/6)]
         [fxoff (* xunit 2)])
    @nested[#:style "tikzpicture"]{
  @(ipa-line fxoff fx0 dx width 0
             (vnode 'a '&OE 'aoe #:ja-vowel 'left)
             (vnode '&ipacentrialized.a #false 'ca #:ja-vowel 'left #:narrow? #true)
             (vnode 'A '6))
  @(ipa-line fxoff fx0 dx width 1 #:draw-line? #false
             (vnode '&ae #false 'ae)
             (vnode '5 #false #:dot? #false))
  @(ipa-line fxoff fx0 dx width 2
             (vnode 'E '&oe 'eoe)
             (vnode '3 '&textcloserevepsilon '3cr3)
             (vnode '2 'O))
  @(ipa-line fxoff fx0 dx width 3 #:draw-line? #false
             (vnode '&textlowering.e '&textlowering.&o 'cmo #:ja-vowel 'left #:narrow? #true)
             (vnode '* #false 'schwa #:dot? #false)
             (vnode #false '&textlowering.o 'mbo #:ja-vowel 'right #:narrow? #true))
  @(ipa-line fxoff fx0 dx width 4
             (vnode 'e '&o 'ecmo #:ja-vowel 'left)
             (vnode '9 '8)
             (vnode '7 'o #:ja-vowel 'right))
  
  @(ipanode (+ (ipa-x fxoff fx0 dx 5) (* xunit 2)) 5 (vnode 'I 'Y))
  @(ipanode (+ width fxoff (* xunit -2)) 5 (vnode #false 'U))
  
  @(ipanode (+ width fxoff (* xunit -2)) 6 (vnode '&textsubplus.W #false 'nbW #:ja-vowel 'left #:narrow? #true))
  
  @(ipa-line fxoff fx0 dx width 6
             (vnode 'i 'y #:ja-vowel 'left)
             (vnode '1 '0)
             (vnode 'W 'u #:ja-vowel 'left))
  
  @(ipa-edge 'aoe 'ae 'eoe 'cmo 'ecmo 'iy)
  @(ipa-edge 'ca '5 '3cr3 'schwa '98 '10)
  @(ipa-edge 'A6 '2O 'mbo '7o 'Wu)
  
  @(for/list ([vlabel (in-list '(Front Central Back))]
              [x (in-naturals 0)])
     (ipanode (+ fxoff (* x width 1/2)) 7 (vnode vlabel #:dot? #false #:elem tt)))
  
  @(for/list ([vlabel (in-list '(Close Near-Close Close-Mid Mid Open-Mid Near-Open Open))]
              [inve-y (in-naturals 0)])
     (ipanode 0 (- 6 inve-y) (vnode vlabel #:dot? #false #:elem tt)))
  })
}

@handbook-scenario{@ja-title[Consonants 子音 しいん 辅音]}

@deftech{Consonant}s are produced with a partial or total closure of the vocal tract.

@handbook-reference[]
