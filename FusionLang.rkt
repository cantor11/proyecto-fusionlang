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
))

;Especificación Sintáctica (gramática)

(define grammar-fusion-lang
  '((program (GLOBALS PROGRAM) a-program)
    
;========================= BLOQUE GLOBALS =========================
    (GLOBALS ("GLOBALS" "{"(arbno decl-global)"}")
             globals-block)
    
    (decl-global (var-declaration)
                 global-var)
    (decl-global (const-declaration)
                 global-const)
    (decl-global (function-declaration)
                 global-function)
    
;========================= DECLARACIONES GLOBALES =========================
    (var-declaration (type-exp identificador "=" expression ";")
                     var-declaration)
    (const-declaration ("const" type-exp identificador "=" expression ";")
                       const-declaration)
   (function-declaration ("proc" identificador "=" "function" "(" type-exp identificador ")" "{"arbno expression "}")
                         function-declaration)

;========================= BLOQUE PROGRAM =========================
    (PROGRAM ("PROGRAM" "{" type-exp-func "main" "(" ")" "{" "return" expression ";" "}" "}")
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
    
;;========================= TIPO DE DATO FUNCION =========================
    (type-exp-func ("proc") proc-type)
    
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
    (primitiva-cad ("longitud") primitiva-longitud)
    (primitiva-ca ("concat") primitiva-concat)

;========================= BLOQUES LOCALS =========================
    (locals-block ("LOCALS" "{" (arbno decl-local) (arbno statement) "}")
                  local-block)

    (decl-local (var-declaration)
                local-var)
    (decl-local (const-declaration)
                local-const)

    (statement (expression ";")
               local-expression
               (BLOCK (arbno statement) "return" expression ";"))

;========================= FUNCIONES DE BLOQUE =========================
    (BLOCK ("BLOCK" "{" (arbno statement) "}")
           block-body)
    ))


(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-fusion-lang))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-fusion-lang))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-fusion-lang)))

;El Interpretador + checker (FrontEnd + Evaluación + señal para lectura )

(define interpretador-tipos
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpretador  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-fusion-lang)))

(define aux-interpretador
  (lambda (x)
    (if (type? (type-of-program x)) (eval-program  x) 'error)))

(define type?
  (lambda (x)
    (member x '(int-type float-type string-type bool-type
                         list-type vector-type dict-type proc-type))))
(define type-of-program
  (lambda (program)
    (cond
      [(and (list? program) (eq? (car program) 'program)) 'proc-type]  ; Si es un 'program', devuelve 'proc-type'
      [else 'unknown-type])))  ; Si no es un 'program', devuelve 'unknown-type'


(define a-program
  (lambda (globals-block program-block)
    ;; Devuelve una estructura de datos representando el programa.
    (list 'program
          (list 'globals globals-block)
          (list 'main-program program-block))))

(define primitiva-suma 'primitiva-suma)
(define primitiva-resta 'primitiva-resta)
(define primitiva-div 'primitiva-div)
(define primitiva-multi 'primitiva-multi)
(define primitiva-comparador-igual 'primitiva-comparador-igual)
(define primitiva-diferente 'primitiva-diferente)
(define primitiva-menor-igual 'primitiva-menor-igual)
(define primitiva-mayor-igual 'primitiva-mayor-igual)
(define primitiva-menor 'primitiva-menor)
(define primitiva-mayor 'primitiva-mayor)
(define primitiva-asignar 'primitiva-asignar)
(define primitiva-append 'primitiva-append)
(define primitiva-cola 'primitiva-cola)
(define primitiva-cabeza 'primitiva-cabeza)
(define primitiva-es-vacio 'primitiva-es-vacio)
(define primitiva-vacio 'primitiva-vacio)
(define primitiva-crear-lista 'primitiva-crear-lista)
(define primitiva-es-lista 'primitiva-es-lista)

;========================= PRIMITIVAS VECTORES =========================
(define primitiva-es-vector 'primitiva-es-vector)
(define primitiva-crear-vector 'primitiva-crear-vector)
(define primitiva-obtener-valor-vector 'primitiva-obtener-valor-vector)
(define primitiva-cambiar-valor-vector 'primitiva-cambiar-valor-vector)
(define primitiva-append-vector 'primitiva-append-vector)
(define primitiva-elimina-valor-vector 'primitiva-elimina-valor-vector)

;========================= PRIMITIVAS DICCIONARIOS =========================
(define primitiva-es-dict 'primitiva-es-dict)
(define primitiva-crear-dict 'primitiva-crear-dict)
(define primitiva-obtener-dict 'primitiva-obtener-dict)
(define primitiva-cambiar-valor-dict 'primitiva-cambiar-valor-dict)
(define primitiva-append-dict 'primitiva-append-dict)
(define primitiva-lista-clave-dict 'primitiva-lista-clave-dict)
(define primitiva-lista-valor-dict 'primitiva-lista-valor-dict)

;========================= PRIMITIVAS CADENAS ========================= 
(define primitiva-longitud 'primitiva-longitud)
(define primitiva-concat 'primitiva-concat)

;=========================== evaluadores semánticos para cada acción ==========================================
(define eval-program
  (lambda (program)
    (let* ([globals (assoc 'globals program)]
           [main (assoc 'main-program program)]
           [env (make-env '())]) ; Entorno global inicial vacío
      (begin
        (eval-globals globals env) ; Procesar las variables/funciones globales
        (eval-main main env)))))  ; Ejecutar el programa principal

(define eval-globals
  (lambda (globals env)
    (for-each
     (lambda (decl)
       (let ((var (car decl))   ; Extraer la variable
             (value (cdr decl)))  ; Extraer su valor
         (env-extend env var value)))  ; Extender el entorno global
     globals)))


(define globals-block
  (lambda (decls)
    ;; Retorna una lista que representa las declaraciones globales
    (list 'globals decls)))

(define global-var
  (lambda (var-decl)
    ;; Representa una declaración de variable global
    (list 'global-var var-decl)))

(define global-const
  (lambda (const-decl)
    ;; Representa una declaración de constante global
    (list 'global-const const-decl)))

(define global-function
  (lambda (func-decl)
    ;; Representa una declaración de función global
    (list 'global-function func-decl)))

(define var-declaration
  (lambda (type name value)
    ;; Representa una declaración de variable
    (list 'var-declaration
          (list 'type type)
          (list 'name name)
          (list 'value value))))

(define const-declaration
  (lambda (type name value)
    ;; Representa una declaración de constante
    (list 'const-declaration
          (list 'type type)
          (list 'name name)
          (list 'value value))))

(define function-declaration
  (lambda (name param-type param-name body)
    ;; Representa una declaración de función
    (list 'function-declaration
          (list 'name name)
          (list 'param-type param-type)
          (list 'param-name param-name)
          (list 'body body))))


(define program-block
  (lambda (main)
    ;; Retorna la estructura del bloque principal
    (list 'program-block main)))

(define numero-lit
  (lambda (num)
    ;; Representa un literal numérico
    (list 'numero-lit num)))

(define cadena-lit
  (lambda (str)
    ;; Representa un literal de cadena
    (list 'cadena-lit str)))

(define var-exp
  (lambda (var)
    ;; Representa una variable
    (list 'var-exp var)))

(define false-exp
  (lambda ()
    ;; Representa un literal booleano False
    (list 'false-exp)))

(define true-exp
  (lambda ()
    ;; Representa un literal booleano True
    (list 'true-exp)))

(define list-exp
  (lambda (elements)
    ;; Representa una lista de expresiones
    (list 'list-exp elements)))

(define vector-exp
  (lambda (elements)
    ;; Representa un vector de expresiones
    (list 'vector-exp elements)))

(define dic-exp
  (lambda (pairs)
    ;; Representa un diccionario
    (list 'dic-exp pairs)))

;Type Expressions

(define int-type
  (lambda ()
    ;; Representa el tipo entero
    (list 'int-type)))

(define float-type
  (lambda ()
    ;; Representa el tipo flotante
    (list 'float-type)))

(define string-type
  (lambda ()
    ;; Representa el tipo cadena
    (list 'string-type)))

(define bool-type
  (lambda ()
    ;; Representa el tipo booleano
    (list 'bool-type)))

(define list-type
  (lambda (element-type)
    ;; Representa el tipo lista con un subtipo
    (list 'list-type element-type)))

(define vector-type
  (lambda (element-type)
    ;; Representa el tipo vector con un subtipo
    (list 'vector-type element-type)))

(define dict-type
  (lambda (key-type value-type)
    ;; Representa el tipo diccionario con tipos de clave y valor
    (list 'dict-type key-type value-type)))

(define proc-type
  (lambda ()
    ;; Representa el tipo de una función
    'proc-type))


;Statements

(define local-expression
  (lambda (expr)
    ;; Representa una expresión en el bloque LOCAL
    (list 'local-expression expr)))

(define block-body
  (lambda (statements)
    ;; Representa el cuerpo de un bloque
    (list 'block-body statements)))

;Locals Block

(define local-block
  (lambda (declarations statements)
    ;; Representa un bloque LOCAL
    (list 'local-block declarations statements)))

(define local-var
  (lambda (var-decl)
    ;; Representa una declaración de variable local
    (list 'local-var var-decl)))

(define local-const
  (lambda (const-decl)
    ;; Representa una declaración de constante local
    (list 'local-const const-decl)))

;Function Blocks
#|  
(define function-declaration
  (lambda (name params body)
    ;; Representa una declaración de función
    (list 'function-declaration name params body)))
|#
;Main Program

(define main-program
  (lambda (body)
    ;; Representa el programa principal
    (list 'main-program body)))


(define check-type
  (lambda (expr expected-type env)
    (let ([actual-type (type-of expr env)])
      (if (equal? actual-type expected-type)
          expr
          (eopl:error "Tipo no coincide. Esperado:" expected-type "Encontrado:" actual-type)))))

(define (my-every? pred lst)
  (cond
    [(null? lst) #t]  ; Si la lista está vacía, asumimos que todos los elementos cumplen la condición
    [(not (pred (car lst))) #f]  ; Si algún elemento no cumple la condición, devuelve #f
    [else (my-every? pred (cdr lst))]))  ; Recursivamente verifica el resto de la lista

(define (my-dict? expr)
  (and (pair? expr)  ; Verifica si 'expr' es una lista
       (my-every? pair? expr)))  ; Verifica si todos los elementos son pares (clave-valor)

(define type-of
  (lambda (expr env)
    (cond
      [(number? expr) 'int]  ; Si es un número, su tipo es 'int'
      [(string? expr) 'string]  ; Si es una cadena, su tipo es 'string'
      [(boolean? expr) 'bool]  ; Si es un booleano, su tipo es 'bool'
      [(symbol? expr) (env-lookup env expr)]  ; Si es una variable, buscar su tipo en el entorno
      [(list? expr) 
       (let ((elem-type (type-of (car expr) env)))  ; Tipo del primer elemento de la lista
         (list-type elem-type))]  ; Retorna un tipo de lista que contiene ese tipo
      [(vector? expr) 
       (let ((elem-type (type-of (vector-ref expr 0) env)))  ; Tipo del primer elemento del vector
         (vector-type elem-type))]  ; Retorna un tipo de vector que contiene ese tipo
      [(my-dict? expr) 
       (let ((key-type (type-of (car (hash-keys expr)) env))
             (value-type (type-of (car (hash-values expr)) env)))
         (dict-type key-type value-type))]  ; Tipo de un diccionario con claves y valores
      [else (eopl:error "Tipo no reconocido para" expr)])))  ; Para cualquier otro caso, error

(define (hash-keys hash)
  (map car hash))  ; Devuelve las claves, que son los primeros elementos de los pares

(define (hash-values hash)
  (map cdr hash))  ; Devuelve los valores, que son los segundos elementos de los pares



;=====================================================================

(define apply-primitive-arit
  (lambda (prim args)
    (cond
      [(not (and (number? (car args)) (number? (cadr args)))) ; <- Nota que aquí hay paréntesis
       (eopl:error "Argumentos no válidos para operación aritmética" args)]
      [else
       (cond
         [(eq? prim 'primitiva-suma) (+ (car args) (cadr args))]
         [(eq? prim 'primitiva-resta) (- (car args) (cadr args))]
         [(eq? prim 'primitiva-div) 
          (if (zero? (cadr args))
              (eopl:error "División entre cero")
              (/ (car args) (cadr args)))]
         [(eq? prim 'primitiva-multi) (* (car args) (cadr args))])])))


(define apply-primitive-log
  (lambda (prim args)
    (cond
      [(eq? prim 'primitiva-mayor) (> (car args) (cadr args))]
      [(eq? prim 'primitiva-menor) (< (car args) (cadr args))]
      [(eq? prim 'primitiva-mayor-igual) (>= (car args) (cadr args))]
      [(eq? prim 'primitiva-menor-igual) (<= (car args) (cadr args))]
      [(eq? prim 'primitiva-diferente) (not (equal? (car args) (cadr args)))]
      [(eq? prim 'primitiva-comparador-igual) (equal? (car args) (cadr args))])))

;; Función para buscar el valor de una variable en el entorno
(define (env-lookup env var)
  (let ((entry (assoc var env)))
    (if entry
        (cdr entry)
        (eopl:error "Variable no definida" var))))

;; Función para eliminar el primer par que coincida con la clave
(define (remove-pair key env)
  (cond
    [(null? env) '()]  ;; Si la lista está vacía, devolvemos una lista vacía
    [(eq? (car (car env)) key) (cdr env)]  ;; Si encontramos el par, lo eliminamos
    [else (cons (car env) (remove-pair key (cdr env)))]))  ;; Si no, continuamos con la lista


;; Función para asignar un valor a una variable en el entorno
(define apply-primitive-asig
  (lambda (prim args env)
    (cond
      [(eq? prim 'primitiva-asignar)
       (let ([var (car args)] 
             [value (cadr args)])
         (let ((existing-entry (assoc var env)))
           (if existing-entry
               (begin
                 ;; Actualiza la variable en el entorno reemplazando el valor
                 (set! env (cons (cons var value) (remove-pair var env)))
                 value)
               (eopl:error "Variable no definida" var))))])))

;; Función para buscar el valor asociado con una clave
(define (hash-ref hash key default)
  (let ((entry (assoc key hash)))  ;; Busca el par (key, value)
    (if entry
        (cdr entry)  ;; Si lo encuentra, devuelve el valor (cdr del par)
        default)))   ;; Si no lo encuentra, devuelve el valor por defecto

;; Función para verificar si una clave existe en el hash
(define (hash-has-key? hash key)
  (not (eq? (hash-ref hash key #f) #f)))  ;; Si la clave no existe, hash-ref devuelve #f


(define apply-primitive-list
  (lambda (prim args)
    (cond
      [(eq? prim 'primitiva-es-vacio) (null? (car args))]  ; Verifica si la lista está vacía
      [(eq? prim 'primitiva-vacio) '()]  ; Retorna una lista vacía
      [(eq? prim 'primitiva-crear-lista) args]  ; Crea una lista con los elementos de 'args'
      [(eq? prim 'primitiva-es-lista) (list? (car args))]  ; Verifica si el primer elemento es una lista
      [(eq? prim 'primitiva-cabeza) (car (car args))]  ; Devuelve la cabeza de la lista
      [(eq? prim 'primitiva-cola) (cdr (car args))]  ; Devuelve la cola de la lista
      [(eq? prim 'primitiva-append) (append (car args) (cadr args))]  ; Une dos listas
      [else (eopl:error "Operación no válida para el primitivo" prim)])))  ; Error si el primitivo no es reconocido

(define (subvector vec start end)
  (vector->list (make-vector (- end start)))
  (vector->list (drop (vector->list vec) start) end))

(define apply-primitive-vector
  (lambda (prim args)
    (cond
      [(eq? prim 'primitiva-es-vector) (vector? (car args))]  ; Verifica si el primer argumento es un vector
      [(eq? prim 'primitiva-crear-vector) (make-vector (car args) (cadr args))]  ; Crea un vector con un tamaño y valor inicial
      [(eq? prim 'primitiva-obtener-valor-vector) (vector-ref (car args) (cadr args))]  ; Obtiene el valor en el índice del vector
      [(eq? prim 'primitiva-cambiar-valor-vector) 
       (begin
         (vector-set! (car args) (cadr args) (caddr args))  ; Cambia el valor en el índice del vector
         (car args))]  ; Retorna el vector modificado
      [(eq? prim 'primitiva-append-vector) 
       (let* ([vec1 (car args)]
              [vec2 (vector (cadr args))])
         (list->vector (append (vector->list vec1) (vector->list vec2))))]  ; Convierte los vectores a listas, las concatena y luego las convierte nuevamente a vector
      [(eq? prim 'primitiva-elimina-valor-vector) 
       (let* ([vec (car args)]
              [pos (cadr args)]
              [size (vector-length vec)])
         (list->vector 
          (append (vector->list (subvector vec 0 pos)) 
                  (vector->list (subvector vec (+ pos 1) size))))) ]  ; Elimina el valor en la posición dada del vector
      [else (eopl:error "Operación no válida para el primitivo" prim)])))  ; Error si el primitivo no es reconocido

(define (hash? x)
  (and (pair? x) (assoc (car x) x)))  ; Verifica si la clave existe en el hash (es decir, una lista asociativa)


(define apply-primitive-dict
  (lambda (prim args)
    (cond
      [(eq? prim 'primitiva-es-dict) 
       (and (pair? (car args))  ; Verifica si es una lista de pares clave-valor
            (my-dict? (lambda (x) (pair? x)) (car args)))]  ; Verifica que todos los elementos son pares clave-valor
      [(eq? prim 'primitiva-crear-dict) '()]  ; Devuelve una lista vacía para simular un diccionario
      [(eq? prim 'primitiva-obtener-dict) 
       (let ((dict (car args))
             (key (cadr args)))
         (assoc key dict))]  ; Busca el valor asociado a la clave en la lista de pares
      [(eq? prim 'primitiva-cambiar-valor-dict)
       (let ((dict (car args))
             (key (cadr args))
             (value (caddr args)))
         (if (assoc key dict)
             (begin
               (set! dict (cons (cons key value) (remove-pair (lambda (x) (eq? key (car x))) dict)))
               dict)
             (eopl:error "Clave no encontrada" key)))]
      [(eq? prim 'primitiva-append-dict)
       (let ((dict (car args))
             (pairs (cadr args)))
         (for-each (lambda (pair)
                     (set! dict (cons pair dict)))
                   pairs)
         dict)]
      [(eq? prim 'primitiva-lista-clave-dict) 
       (map car (car args))]  ; Obtiene las claves del diccionario
      [(eq? prim 'primitiva-lista-valor-dict) 
       (map cdr (car args))])))  ; Obtiene los valores del diccionario


(define apply-primitive-cad
  (lambda (prim args)
    (cond
      [(eq? prim 'primitiva-longitud) (string-length (car args))]
      [(eq? prim 'primitiva-concat) (string-append (car args) (cadr args))]
      [else (eopl:error "Primitivo no reconocido" prim)])))

;Entorno jerárquico para manejar variables locales y globales

(define make-env
  (lambda (parent)
    (cons '() parent))) ; Lista vacía de variables y un puntero al entorno padre

#|  
(define env-lookup
  (lambda (env var)
    (cond
      [(assoc var (car env)) => cdr]
      [(cdr env) (env-lookup (cdr env) var)]
      [else (error "Variable no definida:" var)])))
|#

(define env-extend
  (lambda (env var value)
    (cons (cons var value) (car env))))

(define eval-variable
  (lambda (var env)
    (env-lookup env var)))

;Implementación de bloques

(define foldl
  (lambda (proc init lst)
    (if (null? lst)
        init
        (foldl proc (proc init (car lst)) (cdr lst)))))

(define define-statement?
  (lambda (stmt)
    (and (pair? stmt) (eq? (car stmt) 'define))))

(define if-statement?
  (lambda (stmt)
    (and (pair? stmt) (eq? (car stmt) 'if))))

(define assign-statement?
  (lambda (stmt)
    (and (pair? stmt) (eq? (car stmt) 'assign))))

(define expression-statement?
  (lambda (stmt)
    (not (or (define-statement? stmt)
             (if-statement? stmt)
             (assign-statement? stmt)))))

(define eval-statement
  (lambda (stmt env)
    (cond
      [(define-statement? stmt) (eval-define stmt env)]    ; Evaluar una declaración de definición
      [(if-statement? stmt) (eval-if stmt env)]              ; Evaluar una expresión if
      [(assign-statement? stmt) (eval-assign stmt env)]      ; Evaluar una asignación
      [(expression-statement? stmt) (eval-expression stmt env)] ; Evaluar una expresión
      [else (eopl:error "Instrucción no reconocida" stmt)]))) ; Error si no se reconoce la instrucción

(define eval-define
  (lambda (stmt env)
    (let ((var (car stmt)) (value (cadr stmt)))
      (env-extend env var value))))

(define eval-if
  (lambda (stmt env)
    (let ((test (car stmt))
          (then (cadr stmt))
          (else (caddr stmt)))
      (if (eval-expression test env)
          (eval-expression then env)
          (eval-expression else env)))))

;; Crear un hash vacío (una lista vacía)
(define make-hash (lambda () '()))


;; Actualizar o agregar una clave-valor al hash
(define (hash-set! hash key value)
  (let ((entry (assoc key hash)))  ; Busca la clave en el hash
    (if entry
        (begin
          ;; Actualiza el valor en la lista reemplazando el par clave-valor
          (set! hash (cons (cons key value) (remove-pair entry hash))))
        (set! hash (cons (cons key value) hash)))))  ; Si no existe, agrega el nuevo par


(define eval-assign
  (lambda (stmt env)
    (let ((var (car stmt))
          (value (cadr stmt)))
      (hash-set! env var value))))  ; Actualiza la clave 'var' con el valor 'value' en el hash


(define eval-expression
  (lambda (expr env)
    (cond
      [(number? expr) expr]
      [(symbol? expr) (env-lookup env expr)]
      [else (eopl:error "Expresión no soportada" expr)])))

(define block
  (lambda (statements env)
    (foldl (lambda (stmt _)
             (eval-statement stmt env))
           #f
           statements))) ; Evalúa las instrucciones en secuencia, retorna el último resultado

(define eval-block
  (lambda (block-statements env)
    (block block-statements env)))

;Implementación de locales
(define locals
  (lambda (declarations statements env)
    (let ((local-env (make-env env))) ; Crear un entorno local extendido
      (for-each (lambda (decl)
                  (env-extend local-env (car decl) (cdr decl))) ; Extender variables locales
                declarations)
      (eval-block statements local-env)))) ; Ejecutar las instrucciones en el entorno extendido

(define eval-locals
  (lambda (declarations body env)
    (locals declarations body env)))

;Implementación de la función principal main
(define eval-main
  (lambda (statements env)
    (eval-block statements env)))

;Producción extendida para soportar funciones
(define eval-function
  (lambda (params body env)
    (lambda (args)
      (let ((local-env (make-env env)))
        (for-each (lambda (param arg)
                    (env-extend local-env param arg))
                  params
                  args)
        (eval-block body local-env)))))



;(interpretador)

;(scan&parse "GLOBALS { int a = 10; } PROGRAM { proc main() { return a; } }")