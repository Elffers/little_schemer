#lang racket

(define print_prompt
  (lambda (func)
    
    (fprintf (current-output-port)
           "Example solution for ~s \n" func)))

;;; Chapter 2

(define atom?
  (lambda (x)
  (and (not (pair? x))(not (null? x)))))

  
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))

(lat? '(foo bar))
(eq? 'a 'a)
(car '(a b c))

(define member?
  (lambda(a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(print_prompt "member?")
(member? 'foods '(foo bar))

;;; Chapter 3

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car  lat)
                  (rember a (cdr lat)))))))

(print_prompt "rember")
(rember 'foo '(remove foo baz))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(print_prompt "firsts")
(firsts '((a s) (b e) (c d)))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote())) 
      (else
       (cond
        ((eq? (car lat) old)
         (cons old (cons new (cdr lat))))
        (else (cons (car lat)
                     (insertR new old (cdr lat)))))))))

(print_prompt "insert_R")
(insertR 'e 'd '(a b c d f g))
  
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote())) 
      (else
       (cond
        ((eq? (car lat) old)
         (cons new lat))
        (else (cons (car lat)
                     (insertL new old (cdr lat)))))))))

(print_prompt "insert_L")
(insertL 'd 'e '(a b c e f g))

(define subst
  (lambda(new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons new (cdr lat)))
         (else (cons (car lat)
                     (subst new old (cdr lat)))))))))

(print_prompt "subst")
(subst 'boo'bar '(foo bar baz))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) o1)
          (cons new (cdr lat)))
         ((eq? (car lat) o2)
          (cons new (cdr lat)))
         (else (cons (car lat)
                   (subst2 new o1 o2 (cdr lat)))))))))

(print_prompt "subst2")
(subst2 'vanilla 'chocolate 'banana '(caramel ice cream with chocolate topping))
(subst2 'vanilla'chocolate 'banana '(banana ice cream with chocolate topping))
(subst2 'banana'chocolate 'vanilla '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) a)
          (multirember a (cdr lat)))
         (else (cons (car lat)
                     (multirember a (cdr lat)))))))))

(print_prompt "multirember")
(multirember 'cup '(coffee cup tea cup hick cup))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertR new old (cdr lat)))))))))

(print_prompt "multiinsertR")
(multiinsertR 'foo 'a '(c a b a d))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons new (cons old
              (multiinsertL new old (cdr lat)))))
         (else (cons (car lat)
               (multiinsertL new old (cdr lat)))))))))

(print_prompt "multiinsertL")
(multiinsertL 'foo 'bar '(bar baz bar bax))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons new (multisubst new old (cdr lat))))
         (else
           (cons (car lat) (multisubst new old (cdr lat)))))))))

(print_prompt "multisubst")
(multisubst 'foo 'bar '(bar baz bar bax))

;;; Chapter 4

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (add1 (o+ n (sub1 m)))))))

(print_prompt "o+")
(o+ 2 4)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (sub1 (o- n (sub1 m)))))))

(print_prompt "o-")
(o- 7 3)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup(cdr tup)))))))

(print_prompt "addtup")
(addtup (quote (2 10 4)))

(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (o+ n (multiply n (sub1 m)))))))

(print_prompt "multiply")
(multiply 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and(null? tup1)(null? tup2))
      (quote()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
    (else
     (cons(o+(car tup1) (car tup2))
           (tup+(cdr tup1) (cdr tup2)))))))

(print_prompt "tup+")
(tup+ (quote (1 2 3)) (quote (4 5 6)))
(tup+ (quote (1 2 )) (quote (4 5 6)))
(tup+ (quote (1 2 3)) (quote (4 5)))

(define greater?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
    (else
     (greater? (sub1 n) (sub1 m))))))

(print_prompt "greater?")
(greater? 2 3)
(greater? 3 2)
(greater? 3 3)

(define less?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
    (else
     (less? (sub1 n) (sub1 m))))))

(print_prompt "less?")
(less? 2 3)
(less? 3 2)
(less? 3 3)

(define eq
  (lambda (n m)
    (cond
      ((greater? n m) #f)
      ((less? n m) #f)
      (else #t))))

(print_prompt "eq")
(eq 2 3)
(eq 2 2)

(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
     (else
      (multiply n (power n (sub1 m)))))))

(print_prompt "power")
(power 2 5)
(power 5 3)

(define divide
  (lambda (n m)
    (cond
      ((less? n m) 0)
    (else
     (add1(divide (o- n m) m))))))

(print_prompt "divide")
(divide 2 5)
(divide 5 2)
(divide 15 2)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
    (else
     (add1 (length(cdr lat)))))))

(print_prompt "length")
(length '(1 2 3 4 5))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
    (else
      (pick (sub1 n) (cdr lat))))))

(print_prompt "pick")
(pick 3 '(a b c d e f))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
    (else
       (cons (car lat)(rempick (sub1 n) (cdr lat)))))))

(print_prompt "rempick")
(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
    (else
       (cond
          ((number? (car lat)) (no-nums (cdr lat)))
        (else
          (cons (car lat) (no-nums (cdr lat)))))))))

(print_prompt "no-nums")
(no-nums '(5 pears 6 prunes 7 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
    (else
      (cond
        ((number? (car lat))
         (cons (car lat) (all-nums (cdr lat))))
        (else
           (all-nums(cdr lat))))))))

(print_prompt "all-nums")
(all-nums '(5 pears 6 prunes 7 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2))
       (eq a1 a2))
    (else
     (eq? a1 a2)))))

(print_prompt "eqan?")
(eqan? 1 1)
(eqan? 1 2)
(eqan? 'a 'a)
(eqan? 1 'a)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
    (else
      (cond
        ((eq? (car lat) a)
         (add1 (occur a (cdr lat))))
       (else
         (occur a (cdr lat))))))))

(print_prompt "occur")
(occur 'a '(a b a c a d))
(occur 'cup '(tea cup coffee cup hick cup pick cup))

(define one?
  (lambda (n)
    (eq n 1)))

(print_prompt "one?")
(one? 1)
(one? 2)
(one? 0)

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
    (else
       (cons (car lat)(rempick (sub1 n) (cdr lat)))))))

(print_prompt "rempick2")
(rempick2 3 '(hotdogs with hot mustard))