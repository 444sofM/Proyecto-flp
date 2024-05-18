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
    (primitiva-binaria ("%") primitiva-mod)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("lista?") primitiva-lista?)
    (primitiva-unaria ("vacio?") primitiva-vacio?)
    (primitiva-unaria ("cabeza") primitiva-cabeza)
    (primitiva-unaria ("cola") primitiva-cola)
    (primitiva-unaria ("vector?") primitiva-vector?)

    (expresion ("vector" "("(separated-list expresion ",") ")")
               vector-exp(exps))
    (booleanos-exp ("[" expresion pred-prim expresion "]" )
               pred-prim-exp)
    
    (pred-prim (">")   pred-bigger)
    (pred-prim ("<")   pred-minor)
    (pred-prim (">=")  pred-bigger-equal)
    (pred-prim ("<=")  pred-minor-equal)
    (pred-prim ("==")  pred-equal)
    (pred-prim ("!=")  pred-not-equal)
    (pred-prim ("and") pred-and)
    (pred-prim ("or")  pred-or)

    (booleanos-exp ("not" expresion) oper-un-bool)
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
    (expresion ("crear-vector" expresion expresion)
               crear-vector-exp)
    (expresion ("ref-vector" expresion expresion)
               ref-vector-exp)
    (expresion ("set-vector" expresion expresion expresion)
               set-vector-exp)
       (expresion ("while" booleanos-exp "do" expresion "done")
               while-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "done")
               for-exp)
    (expresion ("print" expresion)
               print-exp)

    (expresion ("registro" "(" (arbno identificador "=" expresion ",") ")")
           registro-exp)
    (expresion ("ref-registro" expresion identificador)
           ref-registro-exp)
    (expresion ("set-registro" expresion identificador expresion)
           set-registro-exp)
;;Grafos

    (expresion ("grafo" "(" expresion expresion ")")
           grafo-exp)
    (expresion ("vertices" "(" (arbno expresion ",") ")")
           vertices-exp)
    (expresion ("aristas" "(" (arbno "(" (separated-list expresion ",") ")" ",") ")")
           aristas-exp)
            
    

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
                       (if (eval-booleanos-exp test-exp env)
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
        (begin   (set! variables-mutables (append variables-mutables ids))
                 (let ((args (eval-rands rands env)))
                   (eval-expression cuerpo (extend-env ids args env)))
                 ))

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
                 (cond
                   [(obtener-var-mutable ids variables-mutables)
                    (setref!
                     (apply-env-ref env ids)
                     (eval-expression exp env))] 
                   [else (eopl:error 'evaluar-expresion
                                     "La variable es constante" )])
                 1))

      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) 
                                              env)
                             (cdr exps)))))

       
      (crear-vector-exp (len val)
                        (make-vector (eval-expression len env)
                                     (eval-expression val env)))
      
      (ref-vector-exp (vec index)
                      (vector-ref (eval-expression vec env)
                                  (eval-expression index env)))
      
      (set-vector-exp
        (vec index val)
        (let ((vec-new (eval-expression vec env)))
          (vector-set! vec-new
                       (eval-expression index env)
                       (eval-expression val env))
          vec-new))

      (vector-exp (exps)
                  (let(( rand (eval-rands exps env)))
                    ( list->vector rand)))
  (while-exp (cond exp)
        (let loop ()
          (when (eval-booleanos-exp cond env)
            (eval-expression exp env)
            (loop))))
     
      (for-exp (var start end body)
               (let ((var-val (eval-expression start env)))
                 (letrec ((loop (lambda (i)
                                  (if (> i (eval-expression end env))
                                      "for exitoso"
                                      (begin
                                        (eval-expression body (extend-env (list var) (list i) env))
                                        (loop (+ i 1)))))))
                   (loop var-val))))
      (print-exp (exp)
                 (println (eval-expression exp env))
                 )
      (registro-exp (ids exps)
              (let ((id-vec (list->vector ids))
                    (exp-vec (list->vector (eval-rands exps env))))
                (vector id-vec exp-vec)))
      (ref-registro-exp (registro id)
                  (let* ((reg-vec (eval-expression registro env))
                         (id-vec (vector-ref reg-vec 0))
                         (exp-vec (vector-ref reg-vec 1))
                         (pos (rib-find-position id (vector->list id-vec))))
                    (if (number? pos)
                        (vector-ref exp-vec pos)
                        (eopl:error 'ref-registro "Campo no encontrado en el registro"))))
      (set-registro-exp (registro id exp)
                  (let* ((reg-vec (eval-expression registro env))
                         (id-vec (vector-ref reg-vec 0))
                         (exp-vec (vector-ref reg-vec 1))
                         (pos (rib-find-position id (vector->list id-vec)))
                         (new-exp-vec (vector-copy exp-vec)))
                    (if (number? pos)
                        (begin
                          (vector-set! new-exp-vec pos (eval-expression exp env))
                          (vector id-vec new-exp-vec))
                        (eopl:error 'set-registro "Campo no encontrado en el registro"))))

      (grafo-exp (vertices aristas)
           (list (eval-expression vertices env)
                 (eval-expression aristas env)))

      (vertices-exp (exps)
              (eval-rands exps env))

      (aristas-exp (exps)
             (eliminar-duplicados-y-ordenar
              (map (lambda (exp)
                     (eval-rands exp env))
                   exps)))
 
     
      )))
;----------Implementando booleanos

(define eval-booleanos-exp
  (lambda (booleanos env)
    (cases booleanos-exp booleanos
      (pred-prim-exp (exp1 pred-prim exp2)
                (let ((args (eval-rands (list exp1 exp2) env)))
                       (apply-pred-prim pred-prim args)))
      (oper-un-bool (pred)
                    (not (eval-expression pred env))))))
; Función auxiliar para imprimir
(define println
  (lambda (val)
    (eopl:printf "~s~%" val)))
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define
  variables-mutables '())

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))
;aplicar predicado booleano
(define apply-pred-prim
  (lambda (prim args)
    (cases pred-prim prim
      (pred-bigger       () (> (car args) (cadr args)))
      (pred-minor        () (< (car args) (cadr args)))
      (pred-bigger-equal () (>= (car args) (cadr args)))
      (pred-minor-equal  () (<= (car args) (cadr args)))
      (pred-equal        () (equal? (car args) (cadr args)))
      (pred-not-equal    () (not (equal? (car args) (cadr args))))
      (pred-and          () (and (car args) (cadr args)))
      (pred-or           () (or (car args) (cadr args)))
      
      )))
                    
;apply-primitiva: <primitiva> <list-of-expression> -> numero
(define apply-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-div () (/ (car args) (cadr args)))
      (primitiva-mod () (remainder (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args))))))

(define apply-primitiva-unaria
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-lista? () (list? args))
      (primitiva-vacio? () (null? args))
      (primitiva-cabeza () (car args))
      (primitiva-cola () (cdr args))
      (primitiva-vector? () (vector? args)))))

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


(define eliminar-duplicados-y-ordenar
  (lambda (aristas)
    (let loop ((aristas aristas)
               (resultado '()))
      (if (null? aristas)
          resultado
          (let* ((arista (car aristas))
                 (arista-ordenada (ordenar-arista arista))
                 (resta (cdr aristas)))
            (if (member arista-ordenada resultado)
                (loop resta resultado)
                (loop resta (cons arista-ordenada resultado))))))))

(define ordenar-arista
  (lambda (arista)
    (let loop ((arista arista)
               (ordenada '()))
      (if (null? arista)
          ordenada
          (loop (cdr arista)
                (insertar-ordenado (car arista) ordenada))))))

(define insertar-ordenado
  (lambda (elemento lista)
    (cond
      ((null? lista) (list elemento))
      ((string<? elemento (car lista))
       (cons elemento lista))
      (else
       (cons (car lista)
             (insertar-ordenado elemento (cdr lista)))))))

(define vector-copy
  (lambda (vec)
    (list->vector (vector->list vec))))

(define len
  (lambda (vector)
    (vector-length vector)))

(define vec
  (lambda (index vector)
    (vector-ref vector index)))

(define obtener-var-mutable
  (lambda (elemento lista)
    (cond
      [(null? lista) #f]
      [else
       (if(eqv? (car lista) elemento) #t
          (obtener-var-mutable elemento (cdr lista)))])))

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
