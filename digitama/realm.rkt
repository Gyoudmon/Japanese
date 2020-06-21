#lang typed/racket/base

(provide exemplify Realm-Fold)
(provide current-realm realm-list default-realm default-fallback-realm)
(provide default-realm-paths default-realm-extension default-fold-realm)
(provide (struct-out realm-info) (struct-out realm-example))

(require racket/path)
(require racket/list)
(require racket/string)

(require digimon/symbol)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Realm-Examples (Listof Realm-Example))
(define-type Realm-Fold (-> Path (Immutable-HashTable Symbol Realm-Examples) (Immutable-HashTable Symbol Realm-Examples)))

(struct realm-info ([fold : Realm-Fold] [extension : Bytes]) #:type-name Realm-Info)
(struct realm-example ([tokens : (Pairof String (Listof String))] [rubies : (Listof String)] [translations : (Listof String)]) #:type-name Realm-Example #:transparent)

(define default-realm : (->* () (Environment-Variables) Symbol)
  (lambda [[envars (current-environment-variables)]]
    (define maybe-default (environment-variables-ref envars #"JAREALM"))
    
    (cond [(not maybe-default) 'default]
          [else (string->symbol (bytes->string/utf-8 maybe-default))])))

(define current-realm : (Parameterof Symbol) (make-parameter (default-realm)))
(define default-fallback-realm : (Parameterof Symbol) (make-parameter 'default))
(define default-realm-paths : (Parameterof (Listof Path-String)) (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define realm-list : (-> (Listof Symbol))
  (lambda []
    (define all : (Listof Symbol)
      (for*/fold ([languages : (Listof Symbol) null])
                 ([realm-root (in-list (default-realm-paths))]
                  #:when (directory-exists? realm-root)
                  [realm.dir : Path (in-list (directory-list realm-root #:build? #false))]
                  #:when (directory-exists? (build-path realm-root realm.dir)))
        (cons (string->symbol (path->string realm.dir)) languages)))
    (sort (remove-duplicates all) symbol<?)))

(define exemplify : (-> Symbol [#:in Symbol] [#:chapter (Option Symbol)] [#:reload? Boolean] Realm-Examples)
  (lambda [ex #:in [realm (current-realm)] #:chapter [chapter #false] #:reload? [reload? #false]]
    (when (or reload? (not (hash-has-key? realm-base realm)))
      (hash-set! realm-base realm (load-realms realm)))
    (define maybe-examples : (Option Realm-Examples)
      (cond [(symbol? chapter) (hash-ref (hash-ref (hash-ref realm-base realm) chapter make-empty-library) ex sorry-for-not-found)]
            [else (for/or : (Option Realm-Examples) ([dialects (in-hash-values (hash-ref realm-base realm))])
                    (hash-ref dialects ex sorry-for-not-found))]))
    (cond [(list? maybe-examples) maybe-examples]
          [(eq? realm (default-fallback-realm)) null]
          [else (exemplify ex #:in (default-fallback-realm) #:chapter chapter #:reload? reload?)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define topic : Symbol 'exn:read:realm)
(define realm-base : (HashTable Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Examples))) (make-hasheq))

(define default-realm-extension : Bytes #".tex")
(define make-empty-library : (-> (Immutable-HashTable Symbol Realm-Examples)) (λ [] (make-immutable-hasheq)))
(define sorry-for-not-found : (-> False) (λ [] #false))

(define default-fold-realm : Realm-Fold
  (lambda [realm.tex library]
    (call-with-input-file* realm.tex
      (λ [[/dev/exin : Input-Port]]
        (let realm-fold : (Immutable-HashTable Symbol Realm-Examples)
          ([library : (Immutable-HashTable Symbol Realm-Examples) library]
           [open? : Boolean #false])
          (define name : (Option Symbol) (realm-read-example-name realm.tex /dev/exin open?))
          (define-values (examples maybe-next) (realm-read-example realm.tex /dev/exin))
          (define library++ : (Immutable-HashTable Symbol Realm-Examples) (if (not name) library (hash-set library name examples)))

          (cond [(not maybe-next) library++]
                [else (realm-fold library++ #true)]))))))

(define load-realms : (-> Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Examples)))
  (lambda [realm]
    (define field : String (symbol->string realm))
    (for/fold ([library : (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Examples)) (make-immutable-hasheq)])
              ([realm-root (in-list (remove-duplicates (default-realm-paths)))]
               #:when (directory-exists? (build-path realm-root field)))
      (define-values (this-realm-fold this-extension)
        (let ([reader.rkt (build-path realm-root "reader.rkt")])
          (cond [(not (file-exists? reader.rkt)) (values default-fold-realm default-realm-extension)]
                [else (let ([& (dynamic-require reader.rkt 'realm-info)])
                        (cond [(realm-info? &) (values (realm-info-fold &) (realm-info-extension &))]
                              [else (values default-fold-realm default-realm-extension)]))])))
      (for/fold ([library : (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Examples)) library])
                ([examples.ext (in-list (directory-list (build-path realm-root field) #:build? #true))]
                 #:when (and (file-exists? examples.ext)
                             (equal? (path-get-extension examples.ext) this-extension)))
        (define subfield : Symbol (string->symbol (format "~a" (file-name-from-path (path-replace-extension examples.ext #"")))))
        (with-handlers ([exn:fail? (λ [[e : exn]] (dtrace-warning #:topic topic "~a: ~a" examples.ext (exn-message e)) library)])
          (hash-update library subfield
                       (λ [[library : (Immutable-HashTable Symbol Realm-Examples)]]
                         (this-realm-fold examples.ext library))
                       make-empty-library))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define realm-read-example-name : (-> Path Input-Port Boolean (Option Symbol))
  (lambda [realm.tex /dev/exin already-start?]
    (let read-id ([srahc : (Option (Listof Char)) (and already-start? null)])
      (define ch : (U Char EOF) (read-char /dev/exin))
      (cond [(eof-object? ch) (and (pair? srahc) (dtrace-warning #:topic topic "~a: ~s" realm.tex (list->string (reverse srahc))) #false)]
            [(eq? ch #\])
             (let ([garbage (read-line /dev/exin)])
               (cond [(pair? srahc) (rlist->symbol srahc)]
                     [else (dtrace-warning #:topic topic "~a: ]~a" realm.tex garbage) #false]))]
            [(eq? ch #\[) (read-id null)]
            [(list? srahc) (read-id (cons ch srahc))]
            [(char-whitespace? ch) (read-id srahc)]
            [else (read-line /dev/exin) (read-id #false)]))))

(define realm-read-example : (->* (Path Input-Port) ((Listof Realm-Example)) (Values (Listof Realm-Example) (Option Char)))
  (lambda [realm.tex /dev/exin [selpmaxe null]]
    (let*-values ([(jatokens maybe-next) (realm-read-example-tokens realm.tex /dev/exin)]
                  [(rubytokens maybe-next) (if (null? jatokens) (values null maybe-next) (realm-read-example-tokens realm.tex /dev/exin))]
                  [(translations maybe-next) (if (null? rubytokens) (values null maybe-next) (realm-read-example-translations realm.tex /dev/exin))])
      (define maybe-example : (Option Realm-Example) (and (pair? jatokens) (realm-example jatokens rubytokens translations)))
      
      (cond [(not maybe-example) (values (reverse selpmaxe) (and (eq? maybe-next #\[) maybe-next))]
            [(not maybe-next) (values (reverse (cons maybe-example selpmaxe)) #false)]
            [(eq? maybe-next #\[) (values (reverse (cons maybe-example selpmaxe)) maybe-next)]
            [else (realm-read-example realm.tex /dev/exin (cons maybe-example selpmaxe))]))))

(define realm-read-example-tokens : (-> Path Input-Port (Values (Listof String) (Option Char)))
  (lambda [realm.tex /dev/exin]
    (let read-tokens ([srahc : (Listof Char) null]
                      [snekot : (Listof String) null])
      (define ch : (U Char EOF) (read-char /dev/exin))
      (cond [(eof-object? ch) (values (reverse snekot) #false)]
            [(or (eq? ch #\return) (eq? ch #\newline))
             (when (and (eq? ch #\return) (eq? (peek-char /dev/exin) #\newline))
               (read-char /dev/exin))
             (cond [(pair? srahc) (values (reverse (cons (list->string (reverse srahc)) snekot)) #false)]
                   [else (read-tokens null snekot)])]
            [(char-whitespace? ch)
             (regexp-match? #px"[[:blank:]]*" /dev/exin)
             (read-tokens null (if (pair? srahc) (cons (list->string (reverse srahc)) snekot) snekot))]
            [(eq? ch #\[) (values (reverse (if (pair? srahc) (cons (list->string (reverse srahc)) snekot) snekot)) ch)]
            [(eq? ch #\|) (read-tokens (append (realm-read-rliteral /dev/exin) srahc) snekot)]
            [else (read-tokens (cons ch srahc) snekot)]))))

(define realm-read-example-translations : (-> Path Input-Port (Values (Listof String) (Option Char)))
  (lambda [realm.tex /dev/exin]
    (let read-translations ([snoitalsnart : (Listof String) null])
      (define ch : (U Char EOF) (peek-char /dev/exin))
      (cond [(eof-object? ch) (values (reverse snoitalsnart) #false)]
            [(eq? ch #\[) (values (reverse snoitalsnart) #\[)]
            [(or (eq? ch #\return) (eq? ch #\newline)) (regexp-match #px"\\s+" /dev/exin) (values (reverse snoitalsnart) (if (eq? (peek-char /dev/exin) #\[) #\[ #\c))]
            [(char-whitespace? ch) (read-char /dev/exin) (read-translations snoitalsnart)]
            [else (let ([raw (assert (read-line /dev/exin) string?)])
                    (read-translations (cons (string-replace (string-trim raw #:left? #false) "|" "") snoitalsnart)))]))))

(define realm-read-rliteral : (->* (Input-Port) ((Listof Char)) (Listof Char))
  (lambda [/dev/exin [laretil null]]
    (define ch : (U Char EOF) (read-char /dev/exin))
    (cond [(eof-object? ch) laretil]
          [(eq? ch #\|) laretil]
          [else (realm-read-rliteral /dev/exin (cons ch laretil))])))
