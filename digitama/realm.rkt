#lang typed/racket/base

(provide exemplify Realm-Fold)
(provide current-realm realm-list default-realm default-fallback-realm)
(provide default-realm-paths default-realm-extension default-fold-realm)
(provide (struct-out realm-info))

(require racket/path)
(require racket/list)
(require racket/string)
(require racket/match)

(require digimon/collection)
(require digimon/system)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Realm-Fold (-> Path (Immutable-HashTable Symbol Realm-Example) (Immutable-HashTable Symbol Realm-Example)))
(struct realm-info ([fold : Realm-Fold] [extension : Bytes]) #:type-name Realm-Info)
(struct realm-example ([tokens : (Listof Symbol)] [rubies : (Listof Symbol)] [translations : (Listof String)]) #:type-name Realm-Example)

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

(define exemplify : (-> Symbol [#:in Symbol] [#:dialect (Option Symbol)] [#:reload? Boolean] (Option Realm-Example))
  (lambda [word #:in [realm (current-realm)] #:dialect [dialect #false] #:reload? [reload? #false]]
    (when (or reload? (not (hash-has-key? dictionary-base realm)))
      (hash-set! dictionary-base realm (load-realms realm)))
    (define maybe-example : (Option Realm-Example)
      (cond [(symbol? dialect) (hash-ref (hash-ref (hash-ref dictionary-base realm) dialect make-empty-dictionary) word sorry-for-not-found)]
            [else (for/or : (Option Realm-Example) ([dialects (in-hash-values (hash-ref dictionary-base realm))])
                    (hash-ref dialects word sorry-for-not-found))]))
    (cond [(realm-example? maybe-example) maybe-example]
          [else (and (not (eq? realm (default-fallback-realm)))
                     (exemplify word #:in (default-fallback-realm) #:dialect dialect #:reload? reload?))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define topic : Symbol 'exn:read:realm)
(define dictionary-base : (HashTable Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Example))) (make-hasheq))

(define default-realm-extension : Bytes #".tex")
(define make-empty-dictionary : (-> (Immutable-HashTable Symbol Realm-Example)) (λ [] (make-immutable-hasheq)))
(define sorry-for-not-found : (-> False) (λ [] #false))

(define default-fold-realm : Realm-Fold
  (lambda [realm.rktl library]
    (define records : Any (with-input-from-file realm.rktl read))
    (cond [(list? records)
           (for/fold ([dict : (Immutable-HashTable Symbol Realm-Example) library])
                     ([record (in-list records)])
             (match record
               [else (dtrace-warning #:topic topic "~a: ~s" realm.rktl record) dict]))]
          [else (dtrace-warning #:topic topic "~a: ~s" realm.rktl records) library])))

(define load-realms : (-> Symbol (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Example)))
  (lambda [realm]
    (define field : String (symbol->string realm))
    (for/fold ([library : (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Example)) (make-immutable-hasheq)])
              ([realm-root (in-list (remove-duplicates (default-realm-paths)))]
               #:when (directory-exists? (build-path realm-root field)))
      (define-values (this-realm-fold this-extension)
        (let ([reader.rkt (build-path realm-root "reader.rkt")])
          (cond [(not (file-exists? reader.rkt)) (values default-fold-realm default-realm-extension)]
                [else (let ([& (dynamic-require reader.rkt 'realm-info)])
                        (cond [(realm-info? &) (values (realm-info-fold &) (realm-info-extension &))]
                              [else (values default-fold-realm default-realm-extension)]))])))
      (for/fold ([library : (Immutable-HashTable Symbol (Immutable-HashTable Symbol Realm-Example)) library])
                ([examples.ext (in-list (directory-list (build-path realm-root field) #:build? #true))]
                 #:when (and (file-exists? examples.ext)
                             (equal? (path-get-extension examples.ext) this-extension)))
        (define dialect : Symbol (string->symbol (format "~a" (file-name-from-path (path-replace-extension examples.ext #"")))))
        (with-handlers ([exn:fail? (λ [[e : exn]] (dtrace-warning #:topic topic "~a: ~a" examples.ext (exn-message e)) library)])
          (hash-update library dialect
                       (λ [[dictionary : (Immutable-HashTable Symbol Realm-Example)]]
                         (this-realm-fold examples.ext dictionary))
                       make-empty-dictionary))))))
