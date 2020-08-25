#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "../digitama/ipa.rkt" "ipa.rkt"))

(require scribble/base)
(require scribble/core)

@require{../digitama/ipa.rkt}
@require{ipa.rkt}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define full-space (string (integer->char #x3000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define phone-elem
   (lambda [phones]
     (larger (larger (ipa-sym phones)))))

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
