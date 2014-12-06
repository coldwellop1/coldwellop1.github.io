#lang plai-typed

;define types
(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (boxof (listof Binding)))

(define mt-env (box empty))

(define (add-env [b : Binding] [e : Env]) : Env
  (begin
    (set-box! e (cons b (unbox e)))
    e))

(define-type ArithE
  [numE (n : number)]
  [idE (id : symbol)]
  [plusE (left : ArithE) (right : ArithE)]
  [beginE (beg : (listof ArithE))]
  [declareE (id : symbol) (value : ArithE)]
  [setE (id : symbol) (value : ArithE)]
  )

;declare lookup function to return value of binding instance
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? (unbox env)) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first (unbox env))))
             (bind-val (first (unbox env)))]
            [else (lookup for (box (rest (unbox env))))])]))

;declare bool lookup function 
;returns true if binding already exists in environment
(define (lookupExist [for : symbol] [env : Env]) : boolean
  (cond
    [(empty? (unbox env)) #f]
    [else (cond
            [(symbol=? for (bind-name (first (unbox env)))) #t]
            [else (lookupExist for (box (rest (unbox env))))])]))

;our parser
(define (parse [s : s-expression]) : ArithE
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    
    [(s-exp-symbol? s) (idE (s-exp->symbol s))]
    
    [(and (s-exp-list? s)
         (eq? (s-exp->symbol (first (s-exp->list s))) 'begin))
           (beginE (map parse (rest (s-exp->list s))))]
   
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(declare) (declareE (second sl) (third sl))]
         [(set) (setE (second sl) (third sl))]
         [else (error 'parse "syntax error")]))]
    
    [else (error 'parse "syntax error")]))
    
;rotate function moves the binding at the front of the environment list to the back and returns the environment
(define (rotate [env : Env]) : Env
  (let ([f (first (unbox env))])
    (box (cons f (reverse (rest (unbox env)))))))

;used for 'set' function, finds the binding instance in the environment, deletes it, and re-adds it to the environment with a different value.
(define (lookupReplace [id : symbol] [b : Binding] [env : Env]) : Env
  (cond
    [(symbol=? id (bind-name (first (unbox env)))) 
     (add-env b (box (rest (unbox env))))]
    [else (lookupReplace id b (rotate env))]))
     
;our interpreter.
(define (interp [a : ArithE] [e : Env]) : (boxof number)
                (type-case ArithE a
                  [numE (n) (box n)]
                  [idE (id) (box (lookup id e))]   ;returns the value of the id in the environment
                  [plusE (l r) (box (+ (unbox (interp l e)) (unbox (interp r e))))]
                  [declareE (id value)    ;adds the new binding to the environment
                            (cond
                              [(lookupExist id e) (error 'parse "id already defined")]
                              [else 
                               (let ([id (unbox (interp value e))]) 
                                 (box (lookup id (add-env id e))))])]
                  [setE (id value)    ;removes old binding instance with this name and replaces it with the new value
                        (cond
                          [(lookupExist id e) 
                               (let ([b (unbox (interp value e))]) 
                                 (box (lookup id (lookupReplace id b e))))]
                          [else (error 'parse "undefined id")])]
                  
                  [beginE (beg) (interp (first (reverse beg)) e)]    ;interprets the last ArithE expression in the list.
                  ))
       

