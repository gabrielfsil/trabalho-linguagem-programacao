#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

;; Estrutura Base
(struct object (class-name fields))
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))

; Define o ambiente de classes como um ambiente vazio
(define class-env '())

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    ;; Ação: Obtém o valor associado à variável 'v' no ambiente Δ.
    [(ast:var v) (deref (apply-env Δ v))]
    ;; Ação: Aplica um método a um objeto.
    ;;   - Obtém os valores dos argumentos.
    ;;   - Obtém o objeto (instância da classe) associado à expressão 'e'.
    ;;   - Aplica o método ao objeto com os argumentos fornecidos.
    ;; Retorna o resultado da aplicação do método.
    [(ast:send e (ast:var mth) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj (value-of e Δ)])
      (apply-method (find-method (object-class-name obj) mth) obj args-with-value))
    ]
    ;; Ação: Chama um método da superclasse.
    ;;   - Obtém os valores dos argumentos.
    ;;   - Obtém o objeto atual 'self' do ambiente Δ.
    ;;   - Aplica o método da superclasse ao objeto atual com os argumentos fornecidos.
    ;; Retorna o resultado da aplicação do método da superclasse.
    [(ast:super (ast:var c) args) 
      (let ([args-with-value (map-value-of args Δ )]
          [obj (apply-env Δ "self")])
      (apply-method (find-method (apply-env Δ "super") (ast:var-name args)) obj args-with-value ))
    ]
    ;; Ação: Obtém o valor associado à variável 'self' no ambiente Δ.
    ;; Retorna o objeto atual.
    [(ast:self) (apply-env Δ "self")]
    ;; Ação: Cria uma nova instância de uma classe.
    ;;   - Obtém os valores dos argumentos.
    ;;   - Obtém a classe correspondente ao nome 'c'.
    ;;   - Cria uma nova instância da classe com campos inicializados como referências nulas.
    ;;   - Aplica o método de inicialização à nova instância com os argumentos fornecidos.
    ;; Retorna a nova instância criada.
    [(ast:new (ast:var c) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj 
            (let* ([class (find-class c)]
                [field-names (class-field-names class)]
                [fields (map (λ (field-name) (newref null)) field-names)])
            (object c fields))
          ])
      (apply-method (find-method c "initialize") obj args-with-value)
      obj)
    ]
    [e (raise-user-error "unimplemented-value-of-construction: " e)]))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    ;; Ação: Atribui o valor da expressão 'e' à variável 'x' no ambiente Δ.
    ;; Retorna o valor 98.
    [(ast:assign (ast:var x) e) (begin (setref! (apply-env Δ x) (value-of e Δ)) 98)]
    ;; Ação: Exibe o valor da expressão 'e' no console.
    ;; Retorna vazio.
    [(ast:print e) 
      (display (value-of e Δ))
      (newline)]
    ;; Ação: Retorna o valor da expressão 'e'.
    [(ast:return e) (value-of e Δ)]
    ;; Ação: Avalia cada declaração no bloco 'stmts' no ambiente Δ.
    ;; Retorna o resultado da última declaração.
    [(ast:block stmts) (for ([s stmts]) (result-of s Δ))]
    ;; Ação: Avalia a expressão 'e' e executa 's1' se verdadeira, caso contrário executa 's2'.
    ;; Retorna o resultado da execução do bloco correspondente.
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    ;; Ação: Executa repetidamente 's' enquanto a expressão 'e' for verdadeira.
    ;; Retorna 'done' quando a expressão 'e' se torna falsa.
    [(ast:while e s) (if (value-of e Δ)
                         (begin
                           (result-of s Δ)
                           (result-of stmt Δ))
                         'done)]
    ;; Ação: Avalia a declaração 's' no ambiente Δ estendido com uma nova referência para a variável 'x'.
    ;; Retorna o resultado da execução da declaração.
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]
    ;; Padrão: (ast:send e (ast:var mth) args)
    ;; Ação: Chama um método na expressão 'e' com argumentos fornecidos.
    ;;   - Obtém os valores dos argumentos.
    ;;   - Obtém o objeto (instância da classe) associado à expressão 'e'.
    ;;   - Aplica o método ao objeto com os argumentos fornecidos.
    ;; Retorna o resultado da aplicação do método.
    [(ast:send e (ast:var mth) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj (value-of e Δ)])
      (apply-method (find-method (object-class-name obj) mth) obj args-with-value))
    ]
    ;; Ação: Chama um método da superclasse.
    ;;   - Obtém os valores dos argumentos.
    ;;   - Obtém o objeto atual 'self' do ambiente Δ.
    ;;   - Aplica o método da superclasse ao objeto atual com os argumentos fornecidos.
    ;; Retorna o resultado da aplicação do método da superclasse.
    [(ast:super (ast:var c) args)
      (let ([args-with-value (map-value-of args Δ )]
          [obj (apply-env Δ "self")])
      (apply-method (find-method (apply-env Δ "super") c) obj args-with-value ))
    ]
    [e (raise-user-error "unimplemented-result-of-construction: " e)]))

(define (add-class class-name class-list)
  (if (class-exists? class-name class-list)
      (raise-user-error "Já existe uma classe com a mesma definição: " class-name)
      (set! class-env (cons (cons class-name class-list) class-env))))

(define (merge-method m-decls super-name fields)
  (append
   (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)
   (class-method-env (find-class super-name))))

(define (create-method super-name fields m-decl)
  (list
   (ast:var-name (ast:method-name m-decl))
   (method (map ast:var-name (ast:method-params m-decl)) (ast:method-body m-decl) super-name fields)))

(define (find-class class-name)
  (let ([class-pair (assoc class-name class-env)])
    (if class-pair
        (cdr class-pair)
        (raise-user-error "Classe não encontrada: " class-name))))

(define (get-field-names fields)
  (map (lambda (field)
         (if (string? field)
             field
             (ast:var-name field)))
       fields))

(define (class-exists? class-name class-list)
  (let ([existing-class (find-class-exists class-name)])
    (and existing-class
         (equal? (class-field-names existing-class) (class-field-names class-list))
         (equal? (class-method-env existing-class) (class-method-env class-list)))))

(define (find-class-exists class-name)
  (let ([maybe-pair (assoc class-name class-env)])
    (if (pair? maybe-pair)
        (cdr maybe-pair)
        #f)))

(define (append-field-names super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc)
               (append acc (list (string-append field "%1")))
               (append acc (list field))))
         self-fields
         super-fields))

(define (apply-method method self args)
  (let* ([args-with-refs (map newref args)]
         [extended-env (extend-env "self" self
                                   (extend-env "super" (method-super-name method)
                                               empty-env))]
         [method-env (bind-vars (method-fields method)
                                       (object-fields self)
                                       extended-env)])
    (result-of (method-body method)
               (bind-vars (method-vars method)
                                 args-with-refs
                                 method-env))))

(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env)))
  env)

(define (find-method class-name method-name)
  (let ([m-env (class-method-env (find-class class-name))])
    (let ([maybe-pair (assoc method-name m-env)])
      (if (pair? maybe-pair)
          (cadr maybe-pair)
          (raise-user-error "Método não encontrado: " method-name)))))

(define (map-value-of exps Δ)
  (map (λ (exp) (value-of exp Δ)) exps))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
        (add-class "object" (class #f '() '()))
        (for ([decl decls])
          (let* ([class-name (ast:var-name (ast:decl-name decl))]
                [super-name (ast:var-name (ast:decl-super decl))]
                [super-fields (class-field-names (find-class super-name))]
                [fields (append-field-names super-fields (get-field-names (ast:decl-fields decl)))]
                [methods (merge-method (ast:decl-methods decl) super-name fields)]
                [class (class super-name fields methods)])
            (add-class class-name class)))
        (result-of stmt init-env))]))