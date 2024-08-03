#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (common-prefix w1 w2 rez)
  (cond
    [(null? w1) rez]
    [(null? w2) rez]
    [(equal? (car w1) (car w2)) (common-prefix (cdr w1) (cdr w2) (append rez (list (car w1))))]
    [else rez]))
  

(define (rest-of-word w1 w2 rez)
  (cond
    [(null? w1) rez]
    [(null? w2) (rest-of-word (cdr w1) '() (append rez (list (car w1))))]
    [(equal? (car w1) (car w2)) (rest-of-word (cdr w1) (cdr w2) rez)]
    [else (rest-of-word (cdr w1) (cdr w2) (append rez (list (car w1))))]))


(define (longest-common-prefix w1 w2)
  [append (list (common-prefix w1 w2 '()))
          (list (rest-of-word w1 (common-prefix w1 w2 '()) '()))
          (list (rest-of-word w2 (common-prefix w1 w2 '()) '()))])


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (helper-prefix words rez)
  (if (collection-empty? (collection-rest words))
      rez
      (helper-prefix (collection-rest words)(common-prefix (collection-first words) (collection-first (collection-rest words)) '()))
      ))

(define (longest-common-prefix-of-collection words)
  (cond
    [(null? words) '()]
    [else (helper-prefix words '())]))


(define (last-cdr List)
  (if (null? (cdr List))
      (car List)
      (last-cdr (cdr List))))

(define (match-pattern-with-label st pattern)
  (cond
    [(not (list? st)) st]
    [(= (length (car (longest-common-prefix (get-branch-label (first-branch st)) pattern))) (length pattern)) #t]
    [(not (list? (get-ch-branch st (car pattern)))) (cons #f '(()))]
     [(null? (car (cdr (longest-common-prefix (get-branch-label (first-branch st)) pattern))))
      (append (list(car (longest-common-prefix (get-branch-label (first-branch st)) pattern)))
            (list (last-cdr (longest-common-prefix (get-branch-label (first-branch st)) pattern)))
              (list (other-branches(first-branch st))))]
    [(not (null? (car (longest-common-prefix (get-branch-label (first-branch st)) pattern))))
     (cons #f (list (car (longest-common-prefix (get-branch-label (first-branch st)) pattern))))]
    [else (match-pattern-with-label (other-branches st) pattern)])
  )


(define (st-has-pattern? st pattern)
  (cond
    ;primul caz
    [(not (list? (match-pattern-with-label st pattern))) #t]
    ;al doilea caz
    [(equal? #f (car (match-pattern-with-label st pattern))) #f]
    ;al treilea caz
    (else (st-has-pattern? (car (cdr (cdr (match-pattern-with-label st pattern)))) (drop pattern (length (car (match-pattern-with-label st pattern)))))))  
  )


(define (get-suffixes text)
  (if (collection-empty? text)
      '()
      (collection-cons text (get-suffixes (cdr text)))))


(define (starts-ch? ch)
  (lambda (word)
    (cond
      [(null? word) #f]
      [(equal? ch (collection-first word)) #t]
      [else #f])))

(define (get-ch-words words ch)
  (collection-filter (starts-ch? ch) words))


(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map cdr suffixes)))


(define elim-prefix
  (lambda (length)
    (lambda (List)
      (drop List length))))

(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes) (collection-map (elim-prefix (length (longest-common-prefix-of-collection suffixes))) suffixes)))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  'your-code-here)


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  'your-code-here)


(define text->ast
  'your-code-here)


(define text->cst
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)