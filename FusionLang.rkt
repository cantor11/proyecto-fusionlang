#lang eopl

;************************************************************
; Kevin Jordan Alzate - 2228507
; Junior Orlando Cantor Arevalo - 2224949
;
; LINK GITHUB >>> https://github.com/cantor11/proyecto-fusionlang
;************************************************************

;========================== Especificacion Lexica y Sintactica ==========================
; La definición BNF para las expresiones del lenguaje:
;<programa> ::= GLOBALS { <decls-global> } PROGRAM { proc main () { return <expresion>; } }

;<decls-global> ::= { <decl-global> ";" }*

;<decl-global> ::= <var-declaration>
;               | <const-declaration>
;               | <function-declaration>

;<var-declaration> ::= <type-exp> <identificador> "=" <expresion> ";"
;<var-declaration> ::= { <type-exp> <identificador> "=" <expresion> ";" }*

;<const-declaration> ::= "const" <type-exp> <identificador> "=" <expresion> ";"
;<const-declaration> ::= { "const" <type-exp> <identificador> "=" <expresion> ";" }*

;<function-declaration> ::= <type-exp> <identificador> "=" "function" "(" <param-list> ")" "{" <expresion> "}"

;<param-list> ::= <identificador> { "," <identificador> }*

;<expresion> ::= <numero>
;             | "\"" <texto> "\""
;             | <identificador>
;             | "False"
;             | "True"
;             | "(" <expresion> { "," <expresion> } ")"
;             | "[" <expresion> { "," <expresion> } "]"
;             | "{" <expresion-pareada> { "," <expresion-pareada> } "}"

;<expresion-pareada> ::= "\"" <texto> "\"" ":" <expresion>

;<type-exp> ::= "int"
;            | "float"
;            | "string"
;            | "bool"
;            | "list" "<" <type-exp> ">"
;            | "vector" "<" <type-exp> ">"
;            | "dict" "<" <type-exp> "," <type-exp> ">"
;            | "proc"

;<primitiva-aritmetica> ::= "+" | "-" | "/" | "*"
;<primitiva-logica> ::= ">" | "<" | ">=" | "<=" | "!=" | "=="
;<primitiva-asignacion> ::= "->"

;<primitiva-lista> ::= "empty?" | "empty" | "make-list" | "list?" | "head" | "tail" | "append"

;<primitiva-vector> ::= "vector?" | "make-vector" | "ref-vector" | "set-vector" | "append-vector" | "delete-val-vector"

;<primitiva-diccionario> ::= "dict?" | "make-dict" | "ref-dict" | "set-dict" | "append-dict" | "keys-dict" | "values-dict"

;<primitiva-cadena> ::= "longitud"
;<primitiva-binaria> ::= "concat"


;**********************************************    Especificación Léxica   *********************************************

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (numero
   (digit (arbno digit) "." digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (texto
   (letter) string)
  (texto
   (letter (arbno (or letter digit "-" ":"))) string)
))

;Especificación Sintáctica (gramática)

(define grammar-fusion-lang
  '((program (GLOBALS PROGRAM) a-program)
    
;========================= BLOQUE GLOBALS =========================
    (GLOBALS ("GLOBALS" "{"(decls-global)"}")
             globals-block)
    
    (decls-global (arbno decl-global ";") mult-decls-global)
    
    (decl-global (var-declaration) global-var)
    (decl-global (const-declaration) global-const)
    (decl-global (function-declaration) global-function)
;========================= DECLARACIONES GLOBALES =========================
    (var-declaration (arbno type-exp identificador "=" expression ";") var-declaration)
    (const-declaration (arbno "const" type-exp identificador "=" expression) const-declaration)
    (function-declaration (arbno type-exp identificador "=" "function" "(" param-list ")" "{" expression "}") function-declaration)
;========================= BLOQUE PROGRAM =========================
    (PROGRAM ("PROGRAM" "{" "proc" "main" "(" ")" "{" "return" expression ";" "}" "}")
             program-block)
;========================= DATOS =========================
    (expression (numero)
                numero-lit)
    (expression ("\"" texto "\"")
               cadena-lit)
    (expression (identificador)
                var-exp)
    (expression ("False")
                false-exp)
    (expression ("True")
                true-exp)
    (expression ("("(separated-list expresion ",") ")")
               list-exp)
    (expression ("["(separated-list expresion ",") "]")
               vector-exp)
    (expression ("{" (separated-list "\"" text "\"" ":" expression ",") "}")
                dic-exp)
;========================= TIPOS DE DATOS PRIMITIVOS =========================
    (type-exp ("int") int-type)
    (type-exp ("float") float-type)
    (type-exp ("string") string-type)
    (type-exp ("bool") bool-type)
;========================= TIPO DE DATOS COMPUESTOS =========================
    (type-exp ("list" "<" type-exp ">") list-type)
    (type-exp ("vector" "<" type-exp ">") vector-type)
    (type-exp ("dict" "<" type-exp "," type-exp ">") dict-type)
    (type-exp ("proc") proc-type)
;========================= PRIMITIVAS ARITMETICAS =========================
    (primitiva-arit ("+") primitiva-suma)
    (primitiva-arit ("-") primitiva-resta)
    (primitiva-arit ("/") primitiva-div)
    (primitiva-arit ("*") primitiva-multi)
;========================= PRIMITIVAS LOGICAS =========================
    (primitiva-log (">") primitiva-mayor)
    (primitiva-log ("<") primitiva-menor)
    (primitiva-log (">=") primitiva-mayor-igual)
    (primitiva-log ("<=") primitiva-menor-igual)
    (primitiva-log ("!=") primitiva-diferente)
    (primitiva-log ("==") primitiva-comparador-igual)
;========================= PRIMITIVAS ASIGNACION =========================
    (primitiva-asig ("->") primitiva-asignar)
;========================= PRIMITIVAS LISTAS =========================
    (primitiva-lista ("empty?") primitiva-es-vacio)
    (primitiva-lista ("empty") primitiva-vacio)
    (primitiva-lista ("make-list") primitiva-crear-lista)
    (primitiva-lista ("list?") primitiva-es-lista)
    (primitiva-lista ("head") primitiva-cabeza)
    (primitiva-lista ("tail") primitiva-cola)
    (primitiva-lista ("append") primitiva-append)
;========================= PRIMITIVAS VECTORES =========================
    (primitiva-vector ("vector?") primitiva-es-vector)
    (primitiva-vector ("make-vector") primitiva-crear-vector)
    (primitiva-vector ("ref-vector") primitiva-obtener-valor-vector)
    (primitiva-vector ("set-vector") primitiva-cambiar-valor-vector)
    (primitiva-vector ("append-vector") primitiva-append-vector)
    (primitiva-vector ("vector?") primitiva-es-vector)
    (primitiva-vector ("delete-val-vector") primitiva-elimina-valor-vector)
;========================= PRIMITIVAS DICCIONARIOS =========================
    (primitiva-dict ("dict?") primitiva-es-dict)
    (primitiva-dict ("make-dict") primitiva-crear-dict)
    (primitiva-dict ("ref-dict") primitiva-obtener-dict)
    (primitiva-dict ("set-dict") primitiva-cambiar-valor-dict)
    (primitiva-dict ("append-dict") primitiva-append-dict)
    (primitiva-dict ("keys-dict") primitiva-lista-clave-dict)
    (primitiva-dict ("values-dict") primitiva-lista-valor-dict)
;========================= PRIMITIVAS CADENAS ========================= 
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-binaria ("concat") primitiva-concat)
    ))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;************************       Tipos de datos para la sintaxis abstracta de la gramática      *************************
;***********************************************************************************************************************

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (arg-texps (list-of type-exp?))
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (result-texps (list-of type-exp?))
;   (proc-names (list-of symbol?))
;   (arg-texpss (list-of type-exp?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?)))

;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim)
;  (zero-test-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;*******************************************    Parser, Scanner, Interfaz     ******************************************
;***********************************************************************************************************************

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;El Interpretador + checker (FrontEnd + Evaluación + señal para lectura )

(define interpretador-tipos
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpretador  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

(define aux-interpretador
  (lambda (x)
    (if (type? (type-of-program x)) (eval-program  x) 'error)))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;************************************************    El Interprete      ************************************************
;***********************************************************************************************************************

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) '())) '())))
                      (empty-env)))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (args-texps ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (result-texps proc-names arg-texpss idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (true-exp ()
                #t)
      (false-exp ()
                 #f))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-test-prim () (zero? (car args))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*********************************************   Definición tipos     **************************************************
;***********************************************************************************************************************

(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?)))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*************************************************   Type Checker     **************************************************
;***********************************************************************************************************************

;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number)
               int-type)
      (true-exp ()
                bool-type)
      (false-exp ()
                 bool-type)
      (var-exp (id)
               (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
                   (type-of-application
                    (type-of-primitive prim)
                    (types-of-expressions rands tenv)
                    prim rands exp))
      (app-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (let-exp (ids rands body)
               (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv)))))

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))

;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;type-of-proc-exp: (list-of <type-exp>) (list-of <symbol>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión de creación de procedimiento
(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))))

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim ()
                (proc-type (list int-type int-type) int-type))
      (substract-prim ()
                      (proc-type (list int-type int-type) int-type))
      (mult-prim ()
                 (proc-type (list int-type int-type) int-type))
      (incr-prim ()
                 (proc-type (list int-type) int-type))
      (decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type)))))

;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

;type-of-primitive: (list-of <symbol>) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión let
(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))

;type-of-primitive: (list-of <type-exp>) (list-of <symbol>) (list-of (list-of <type-exp>)) (list-of (list-of <symbol>)) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión letrec
(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-type-expressions texps))
                           texpss))
          (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;*********************************************     Procedimientos     **************************************************
;***********************************************************************************************************************

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;***********************************************     Ambientes     *****************************************************
;***********************************************************************************************************************

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
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
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;********************************************  Ambientes de tipos  *****************************************************
;***********************************************************************************************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;****************************************************  Tipos  **********************************************************
;***********************************************************************************************************************

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

;***********************************************************************************************************************
;***********************************************************************************************************************



;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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

;***********************************************************************************************************************
;***********************************************************************************************************************


;***********************************************************************************************************************
;***************************************************    Pruebas    *****************************************************
;***********************************************************************************************************************

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let fun1 = proc((bool -> int) a,
                             (bool -> int) b, bool t,
                             (int -> bool) f, int x)
                            if t then (a (f x))
                            else (b (f x))
             in
                let fun2 = proc(int x, int y)
                              proc(bool z)
                                 if z then +(x, y) else *(x,y)
                    fun3 = proc(int w)
                              zero?(w)
                in
                   (fun1 (fun2 j k) (fun2 m 8) true fun3 4)")

(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))
