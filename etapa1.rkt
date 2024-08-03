#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))



; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

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

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (helper-prefix words rez)
  (if (null? (cdr words))
      rez
      (helper-prefix (cdr words)(common-prefix (car words) (car (cdr words)) '()))
      ))

(define (longest-common-prefix-of-list words)
  (cond
    [(null? words) '()]
    [(equal? 1 (length words)) (car words)]
    [(null? (cdr words)) (cdr words)]
    [else (helper-prefix words '())]))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define stree-1
  '(((#\$))
    ((#\a) ((#\$))
           ((#\n #\a) ((#\$))
                      ((#\n #\a #\$))))
    ((#\b #\a #\n #\a #\n #\a #\$))
    
    ((#\n #\a #\s) ((#\$))
    
    
    
    
    
               ((#\n #\a #\$)))))

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



; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (cond
    ;primul caz
    [(not (list? (match-pattern-with-label st pattern))) #t]
    ;al doilea caz
    [(equal? #f (car (match-pattern-with-label st pattern))) #f]
    ;al treilea caz
    (else (st-has-pattern? (car (cdr (cdr (match-pattern-with-label st pattern)))) (drop pattern (length (car (match-pattern-with-label st pattern)))))))  
  )