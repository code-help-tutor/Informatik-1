;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname lambda) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Prelude

(: compose ((%b -> %c) (%a -> %b) -> (%a -> %c)))
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(: until ((%a -> boolean) (%a -> %a) %a -> %a))
(define until
  (lambda (done? f x)
    (if (done? x)
        x
        (until done? f (f x))))) 

(: repeat (natural (%a -> %a) -> (%a -> %a)))
(define repeat
  (lambda (n f)
    (cond ((= n 0) (lambda (x) x))
          ((> n 0) (compose f (repeat (- n 1) f))))))

; Infrastruktur: Streams

; Erzeuge Versprechen (promise), den Ausdruck e später auswerten zu können
; (delay e) ≡ (lambda () e)
(define promise
  (lambda (v)
    (signature (-> v))))

; Versprechen einlösen: Auswertung des promise p erzwingen
(: force ((promise %a) -> %a))
(define force
  (lambda (p)
    (p)))

; Polymorphe Paare (isomorph zu `pair')
(: make-cons (%a %b -> (cons-of %a %b)))
(: head ((cons-of %a %b) -> %a))
(: tail ((cons-of %a %b) -> %b))
(define-record-procedures-parametric cons cons-of
  make-cons 
  cons?
  (head
   tail))

; Ein Stream besteht aus
; - einem ersten Element (head)
; - einem Promise, den Rest des Streams generieren zu können (tail)
(define stream-of
  (lambda (t)
    (signature (cons-of t (promise (stream-of t))))))

; Sammle erste n Element des Streams in Liste auf
(: stream-take (natural (stream-of %a) -> (list-of %a)))
(define stream-take
  (lambda (n s)
    (if (= n 0) 
        empty
        (make-pair (head s) 
                   (stream-take (- n 1) (force (tail s)))))))

; Infrastruktur: Operationen auf Mengen von Variablennamen

; Charakteristische Funktionen als Repräsentation von
; Mengen von Werten der Signatur t
(define set-of
  (lambda (t)
    (signature (t -> boolean))))

; Leere Menge
(: empty-set (set-of %a))
(define empty-set
  (lambda (x)
    #f))

; Ist Element x in der Menge S?
(: set-member? (%a (set-of %a) -> boolean))
(define set-member?
  (lambda (x S)
    (S x)))

; Element x in Menge S einfügen
(: set-insert (%a (set-of %a) -> (set-of %a)))
(define set-insert
  (lambda (x S)
    (lambda (y)
      (or (equal? y x)
          (S y)))))

; Element x aus Menge S löschen
(: set-delete (%a (set-of %a) -> (set-of %a)))
(define set-delete
  (lambda (x S)
    (lambda (y)
      (if (equal? y x)
          #f
          (S y)))))

; S ∪ T
(: set-union ((set-of %a) (set-of %a) -> (set-of %a)))
(define set-union
  (lambda (S T)
    (lambda (x)
      (or (S x) (T x)))))

; S \ T
(: set-difference ((set-of %a) (set-of %a)-> (set-of %a)))
(define set-difference
  (lambda (S T)
    (lambda (x)
      (and (S x) (not (T x))))))


; Die Syntax des λ-Kalküls

; Stellt e einen gültigen Variablennamen dar (Menge V)?
(: var? (any -> boolean))
(define var?
  (lambda (e)
    (and (symbol? e)
         (not (equal? e 'λ)))))

; Stellt e einen gültigen Lambda-Ausdruck dar (Menge E)?
(: λ-term? (any -> boolean))
(define λ-term?
  (lambda (e)
    (match e
      ((list 'λ v e1) (and (var? v) (λ-term? e1)))        ; (λv.e1)
      ((list e1 e2)   (and (λ-term? e1) (λ-term? e2)))    ; (e1 e2)
      (v              (var? v)))))                        ; v

; Signatur, die die Repräsentation syntaktisch korrekter
; λ-Ausdrücke erkennt
(define λ-term
  (signature (predicate λ-term?)))

; Menge der freien Variablen in e
(: free (λ-term -> (set-of symbol)))
(define free
  (lambda (e)
    (match e
      ((list 'λ v e1) (set-delete v (free e1)))
      ((list e1 e2)   (set-union (free e1) (free e2)))
      (v              (set-insert v empty-set)))))


; Menge der gebundenen Variablen in e
(: bound (λ-term -> (set-of symbol)))
(define bound
  (lambda (e)
    (match e
      ((list 'λ v e1) (set-insert v (bound e1)))
      ((list e1 e2)   (set-union (bound e1) (bound e2)))
      (v              empty-set))))


; Kommt Variable v in e frei vor?
(: free? (symbol λ-term -> boolean))
(define free?
  (lambda (v e)
    (set-member? v (free e))))

; Unendlicher Vorrat (Stream) an Variablennamen, beginnend mit xₙ
; (name-pool 0): x0, x1, x2, ...
(: name-pool (natural -> (stream-of symbol)))
(define name-pool
  (lambda (n)
    (make-cons
     (string->symbol (string-append "x" (number->string n)))
     (lambda () (name-pool (+ n 1))))))

; Erzeuge neuen Variablennamen, der nicht in Menge vs vorkommt
(: fresh ((set-of symbol) -> symbol))
(define fresh
  (lambda (vs)
    (head
     (until 
      (lambda (np) (not (set-member? (head np) vs)))
      (compose force tail)
      (name-pool 0)))))

; e{x￫a}: Ersetze freie Vorkommen von x in e durch a
;         (verhindert Variable Capture)
(: subst (λ-term λ-term symbol -> λ-term))
(define subst
  (lambda (e a x)
    (match e
      ((list 'λ v e1)
       (cond ((equal? v x)
              e)                                              ; (→₄)
             ((not (free? v a))
              (list 'λ v (subst e1 a x)))                     ; (→₅)
             (else
              (let ((z (fresh (set-union (free e) (free a)))))
                (subst (list 'λ z (subst e1 z v)) a x)))))    ; (→₆)
      ((list e1 e2) (list (subst e1 a x) (subst e2 a x)))     ; (→₃)
      (v            (if (equal? v x)
                        a                                     ; (→₁)
                        v)))))                                ; (→₂)

; Die Kombinatoren S, K, I 
; I ≡ (λx.x)
; K ≡ (λx.(λy.x))
; S ≡ (λf.(λg.(λx.((f x) (g x)))))
(: I λ-term)
(define I
  '(λ x x))

(: K λ-term)
(define K
  '(λ x (λ y x)))

(: S λ-term)
(define S
  '(λ f (λ g (λ x ((f x) (g x))))))

; Werte Ausdruck e mittels Strategie Applicative Order aus
; Wurde für das Übungsblatt 13 erweitert, um SKI-Kombinatoren als Symbole zu erlauben.
(: ao (λ-term -> λ-term))
(define ao
  (lambda (e)
    (match e
      ((list 'λ v e1) (list 'λ v (ao e1)))
      ((list e1 e2)   (let ((f (ao e1)))
                        (match f
                          ((list 'λ v e) (ao (subst e (ao e2) v)))
                          (_             (list f (ao e2))))))
      ('S             S)
      ('K             K)
      ('I             I)
      (v              v))))

; Reduktionsstrategie Normal Order (no),
; reduziert Funktionsanwendung VOR Reduktion des Argumentes

; Werte Ausdruck e mittels Strategie Call by Name aus
; Wurde für das Übungsblatt 13 erweitert, um SKI-Kombinatoren als Symbole zu erlauben.
(: bn (λ-term -> λ-term))
(define bn
  (lambda (e)
    (match e
      ((list 'λ v e1) e)
      ((list e1 e2)   (let ((f (bn e1)))
                        (match f
                          ((list 'λ v e) (bn (subst e e2 v)))
                          (_             (list f e2)))))
      ('S             S)
      ('K             K)
      ('I             I)
      (v              v))))


; Werte Ausdruck e mittels Strategie Normal Order aus
; Wurde für das Übungsblatt 13 erweitert, um SKI-Kombinatoren als Symbole zu erlauben.
(: no (λ-term -> λ-term))
(define no
  (lambda (e)
    (match e
      ((list 'λ v e1) (list 'λ v (no e1)))
      ((list e1 e2)   (let ((f (bn e1)))
                        (match f
                          ((list 'λ v e) (no (subst e e2 v)))
                          (_             (list (no f) (no e2))))))
      ('S             S)
      ('K             K)
      ('I             I)
      (v              v))))

; Programmieren im λ-Kalkül

; (1) Booleans
;
; TRUE  ≣ (λx.(λy.x))  ; ignoriere zweites Argument, liefere erstes (Kombinator K)
; FALSE ≣ (λx.(λy.y))  ; ignoriere erstes Argument, liefere zweites
(: TRUE_ λ-term)
(define TRUE_
  '(λ x (λ y x)))
(: FALSE_ λ-term)
(define FALSE_
  '(λ x (λ y y)))

; Fallunterscheidung 
(: IF-ELSE λ-term)
; IF-ELSE ≣ (λx.x)
(define IF-ELSE '(λ x x))

; AND repräsentiert Boolesche Operation ∧
; für die Codierung FALSE, TRUE der Booleans
; AND ≣ (λa.(λb.((b a) b)))
(: AND_ λ-term)
(define AND_
  '(λ a (λ b ((b a) b))))

; ----------------------------------------------------------------------
; (2) Paare
;
; Konstruktion von Paaren ⟨x,y⟩
; PAIR ≣ (λx.(λy.(λs.((s x) y))))
(: PAIR_ λ-term)
(define PAIR_
  '(λ x (λ y (λ s ((s x) y)))))

; Zugriff auf erste Komponente eines Paares p
; FST ≣ (λp.(p (λx.(λy.x))))
(: FST λ-term)
(define FST
  '(λ p (p (λ x (λ y x)))))

; Zugriff auf zweite Komponente eines Paares p
; SND ≣ (λp.(p (λx.(λy.y))))
(: SND λ-term)
(define SND
  '(λ p (p (λ x (λ y y)))))

; ----------------------------------------------------------------------
; (3) Listen: (MAKE-PAIR, FIRST, REST, EMPTY)
;
; MAKE-PAIR ≣ (λx.(λxs.((PAIR FALSE) ((PAIR x) xs))))  ≡ ⟨FALSE,⟨x,xs⟩⟩
; FIRST     ≣ (λxs.(FST (SND xs)))                     
; REST      ≣ (λxs.(SND (SND xs)))                     
; EMPTY?    ≣ FST               ⟨FALSE,⟨x,xs⟩⟩  ⟿  FALSE 
; EMPTY     ≣ I                 (EMPTY? EMPTY) ⟿ (I TRUE)  ⟿  TRUE

(: MAKE-PAIR_ λ-term)
(define MAKE-PAIR_
  (list 'λ 'x (list 'λ 'xs (list (list PAIR_ FALSE_)
                                 (list (list PAIR_ 'x) 'xs)))))
(: FIRST_ λ-term)
(define FIRST_
  (list 'λ 'xs (list FST (list SND 'xs))))

(: REST_ λ-term)
(define REST_
  (list 'λ 'xs (list SND (list SND 'xs))))

(: EMPTY?_ λ-term)
(define EMPTY?_ FST)

(: EMPTY_ λ-term)
(define EMPTY_ '(λ x x))

; Konstruiere Repräsentation der Racket-Liste xs im λ-Kalkül
(: list->λ-list ((list-of λ-term) -> λ-term))
(define list->λ-list
  (lambda (xs)
    (fold EMPTY_ (lambda (y ys) (list (list MAKE-PAIR_ y) ys)) xs)))

; Konstruiere Church Numeral n˜
(: CHURCH (natural -> λ-term))
(define CHURCH
  (lambda (n)
    (list 'λ 'f
          (list 'λ 'x
                ((repeat n (lambda (e) (list 'f e))) 'x)))))

(: SUCC λ-term)
; Wende Church Numeral n auf f und x an; dann noch eine weitere
; Applikation von f:
; SUCC ≣ (λn.(λf.(λx.(f ((n f) x)))))
(define SUCC
  '(λ n (λ f (λ x (f ((n f) x))))))

(: ADD λ-term)
; Schalte die n-fache und m-fache Anwendung von f auf x
; hintereinander:
; ADD ≣ (λn.(λm.(λf.(λx.((n f) ((m f) x))))))
(define ADD 
  '(λ n (λ m (λ f (λ x ((n f) ((m f) x)))))))


(: MULT λ-term)
; Wende Church Numeral m n-fach an:
; MULT ≣ (λn.(λm.(λf.(λx.((n (m f)) x)))))
(define MULT 
  '(λ n (λ m (λ f (λ x ((n (m f)) x))))))

(: ISZERO? λ-term)
; n = 0~: 0-fache Anwendung von (λv.FALSE), liefere TRUE zurück (s. (CHURCH 0))
; n > 0~: n-fache Anwendung von (λv.FALSE) auf TRUE, liefert FALSE
; ISZERO? ≣ (λn.((n (λv.FALSE)) TRUE))
(define ISZERO?
  (list 'λ 'n (list (list 'n 
                          (list 'λ 'v FALSE_)) 
                    TRUE_)))

; Idee ("The Wisdom Teeth Trick"): 
; Für (PRED n~): wende Funktion NEXT (⟨_,x⟩ → ⟨x,x+1⟩) n-mal auf das Paar
; ⟨0,0⟩ an, dann selektiere die erste Komponente des Paares
(: NEXT λ-term)
(define NEXT
  (list 'λ 'p 
        (list (list PAIR_ (list SND 'p))
              (list SUCC  (list SND 'p)))))
                    
; PRED ≣ (λn.(FST ((n NEXT) ((PAIR 0~) 0~))))
(: PRED λ-term)
(define PRED
  (list 'λ 'n
        (list FST
              (list (list 'n NEXT)
                    (list (list PAIR_ (CHURCH 0))   ; ⟨0,0⟩
                                      (CHURCH 0))))))

; Y ≡ (λf.((λx.(f (x x))) (λx.(f (x x)))))
(: Y λ-term)
(define Y
  (let ((xfxx '(λ x (f (x x)))))
    (list 'λ 'f (list xfxx xfxx))))

; ao*: Applicative Order Reduktion mit
; Tracing der Reduktionsschritte in der REPL
;
; Beispiel:
;
; > (ao* print (list (list AND_ TRUE_) FALSE_))
; (((λa.(λb.((b a) b))) (λx.(λy.x))) (λx.(λy.y)))
; ((λb.((b (λx.(λy.x))) b)) (λx.(λy.y)))
; (((λx.(λy.y)) (λx.(λy.x))) (λx.(λy.y)))
; ((λy.y) (λx.(λy.y)))
; (λ x (λ y y))
; > █

(: print (λ-term -> %void))
(define print
  (lambda (e)
    (letrec ((pp (lambda (e)
                   (match e
                     ((list 'λ v e1) (string-append "(λ" (pp v) "." (pp e1) ")"))
                     ((list e1 e2)   (string-append "(" (pp e1) " " (pp e2) ")"))
                     (v              (symbol->string v))))))
      (begin
        (write-string (pp e))
        (write-string "\n")))))
      

(: subst* ((λ-term -> %void) λ-term λ-term symbol -> λ-term))
(define subst*
  (lambda (c e a x)
    (begin
      (c (list (list 'λ x e) a))
      (subst e a x))))

; Wurde für das Übungsblatt 13 erweitert, um SKI-Kombinatoren als Symbole zu erlauben.
(: ao* ((λ-term -> %void) λ-term -> λ-term))
(define ao*
  (lambda (c e)
    (match e
      ((list 'λ v e1) (list 'λ v (ao* (compose c (lambda (e) (list 'λ v e))) e1)))
      ((list e1 e2)   (let* ((f   (ao* (compose c (lambda (e) (list e e2))) e1))
                             (e2_ (ao* (compose c (lambda (e) (list f e ))) e2)))
                        (match f
                          ((list 'λ v e) (ao* c (subst* c e e2_ v)))
                          (_             (list f e2_)))))
      ('S             S)
      ('K             K)
      ('I             I)
      (v              v))))
