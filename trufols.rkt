#lang eopl

;; Ana Sofia Mezu 2225958
;; Leandro Acevedo 2041437
;; https://github.com/leandroace/Taller-3-FLP

;******************************************************************************************

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <un-programa (exp)>

;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= <"\"" <texto> "\"">
;;                      <texto-lit (txt)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <(<expresion> <primitiva-binaria> <expresion>)>
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <primitiva-unaria> (<expresion>)>
;;                      <primapp-un-exp (prim-unaria exp)>

;; <primitiva-binaria> :=  + (primitiva-suma)
;;                     :=  ~ (primitiva-resta)
;;                     :=  / (primitiva-div)
;;                     :=  * (primitiva-multi)
;;                     :=  concat (primitiva-concat)

;;<primitiva-unaria>   :=  longitud (primitiva-longitud)
;;                     :=  add1 (primitiva-add1)
;;                     :=  sub1 (primitiva-sub1)
;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("#" (arbno (not #\newline))) skip)
  (texto
   ((or letter  ":" "!" "$" "_" "-" "|" "%" "&" "°" "<" ">" "^")
    (arbno (or letter digit ":" "!" "$" "_" "-" "|" "%" "&" "°" "<" ">" "^"))) string)
  (identificador
   ("@" letter (arbno (or letter digit))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
  )


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (identificador) var-exp)    
    (expresion
     ("("  expresion primitiva-binaria expresion ")")
     primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion ")") primapp-un-exp)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("lista?") primitiva-lista?)

    
    ;;;;;;;;;; CONDICIONALES ;;;;;;;;;;
    
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI")
                condicional-exp)

    ;;;;;;;;;; DECLARAR VARIABLES ;;;;;;;;;;
    (expresion ("declarar" "("(separated-list identificador "=" expresion ";" ) ")" "{" expresion "}")
                variableLocal-exp)

    ;;;;;;;;;; CREAR PROCEDIMIENTOS ;;;;;;;;;;
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc")
                procedimiento-exp)
    ;;;;;;;;;; EVALUAR PROCEDIMIENTOS ;;;;;;;;;;
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
                app-exp)
    
    ;;;;;;;;;; RECURSIVOS ;;;;;;;;;;
    (expresion ("funcionRec" (arbno identificador "(" (separated-list identificador ";") ")" "=" expresion)
                             "haga" expresion "finRec")
               recursivo-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    (expresion ("lista" "("(separated-list expresion ",") ")")
               list-exp(exps))

    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)
           def-var-exp)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion)
        def-const-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                def-rec-exp)

    (expresion ("begin" expresion (arbno ";" expresion) "end")
                begin-exp)
    (expresion ("set" identificador "=" expresion)
                set-exp)

    (expresion ("\"" texto "\"") texto-lit)))
    ;;;;;;    

;Tipos de datos para la sintaxis abstracta de la gramática construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "PyGraph--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

;*******************************************************************************************

; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @pi)
     '(1 2 3 "hola" "FLP" 3.1416)
     (empty-env))))

;*******************************************************************************************

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expresion exp

      (texto-lit (txt) txt)
      
      (numero-lit (num) num)
      
      (var-exp (id) (apply-env env id))
      (true-exp ()
                #t)
      (false-exp ()
                 #f)
      
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((args (eval-rands (list exp1 exp2) env)))
                         (apply-primitiva-binaria prim-binaria args)))
      
      (primapp-un-exp (prim-unaria exp)
                      (apply-primitiva-unaria prim-unaria (eval-expression exp env)))
      
      (condicional-exp (test-exp true-exp false-exp)
                       (if (eval-expression test-exp env)
                           (eval-expression true-exp env)
                           (eval-expression false-exp env)))
      
      (variableLocal-exp (ids rands cuerpo)
                         (let ((args (eval-rands rands env)))
                           (eval-expression cuerpo
                                            (extend-env ids args env))))
      
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo env))
      
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      
      (recursivo-exp (proc-names idss cuerpos letrec-body)
                     (eval-expression letrec-body
                                      (extend-env-recursively proc-names idss cuerpos env)))
      (list-exp (exps)
                    (eval-rands exps env))
      (def-var-exp (ids rands cuerpo)
             (let ((args (eval-rands rands env)))
               (eval-expression cuerpo
                                (extend-env ids args env))))

      (def-const-exp (ids rands cuerpo)
                (let ((args (eval-rands rands env)))
                  (eval-expression cuerpo
                                   (extend-env ids args env))))
      (def-rec-exp (ids idss rands cuerpo)
             (let ((procs (map cerradura idss rands env)))
               (eval-expression cuerpo
                                (extend-env ids procs env))))
      (set-exp (ids exp)
               (begin
                 (setref!
                  (apply-env-ref env ids)
                  (eval-expression exp env))
                 1))
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
     
      )))



; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitiva: <primitiva> <list-of-expression> -> numero
(define apply-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-div () (/ (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args))))))

(define apply-primitiva-unaria
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-lista? () (list? args)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (cuerpo expresion?)
   (env environment?)
   )
  )

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (lista-ID cuerpo env)
               (eval-expression cuerpo (extend-env lista-ID args env))))))

;*******************************************************************************************
;Ambientes


;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define const? 
  (lambda (sym syms)
    (member sym syms)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
(interpretador)

;                                          PUNTOS CALIFICABLES
; *****************************************************************************************************************

;; a) 


; Ejemplo con radio = 2.5    
;  declarar (
;          @radio=2.5;
;          @areaCirculo = procedimiento(@r) haga ((@pi * @r) * @r) finProc
;          ) {
;             evaluar @areaCirculo (@radio) finEval
;                     }

; b) 


; Ejemplo con 5    
;
;   funcionRec
;          @factorial(@n) = 
;             Si @n 
;                entonces (@n * evaluar @factorial (sub1(@n)) finEval)
;                sino 1 finSI
;          haga
;             evaluar @factorial (5) finEval finRec


; Ejemplo con 10    
;
;   funcionRec
;          @factorial(@n) = 
;             Si @n 
;                entonces (@n * evaluar @factorial (sub1(@n)) finEval)
;                sino 1 finSI
;          haga
;             evaluar @factorial (10) finEval finRec

; c) 


; Ejemplo sumando 4 y 5    
;
;  funcionRec
;    @sumar(@x;@y) = Si @x entonces evaluar @sumar (sub1(@x),add1(@y)) finEval sino @y finSI
;    haga
;    evaluar @sumar (4,5) finEval
;  finRec

; d)


; Ejemplo restando 10 y 3    

;  funcionRec
;    @restar(@x;@y) = Si @y entonces evaluar @restar (sub1(@x),sub1(@y)) finEval sino @x finSI
;    @sumar(@a;@b) = Si @a entonces evaluar @sumar (sub1(@a),add1(@b)) finEval sino @b finSI
;    @multiplicar(@c;@d) = Si @d entonces evaluar @sumar (@c, evaluar @multiplicar (@c,sub1(@d)) finEval) finEval sino 0 finSI
;    haga
;    evaluar @restar (10,3) finEval
;  finRec

; e) 

; declarar (
;    @integrantes = procedimiento () haga "Leandro-y-Ana" finProc;
;    @saludar = procedimiento (@funcion) haga ("Hola:" concat evaluar @funcion () finEval) finProc
;   ) {
;      declarar (@decorate = procedimiento () haga evaluar @saludar (@integrantes) finEval finProc
;        ) {
;           evaluar @decorate () finEval }}

;f) 


; declarar (
;    @integrantes = procedimiento () haga "Leandro-y-Ana" finProc;
;    @saludar = procedimiento (@funcion) haga ("Hola:" concat evaluar @funcion () finEval) finProc
;  ) {
;      declarar (@decorate = procedimiento (@var) haga (evaluar @saludar (@integrantes) finEval concat @var) finProc){
; evaluar @decorate ("-ProfesoresFLP") finEval }}
