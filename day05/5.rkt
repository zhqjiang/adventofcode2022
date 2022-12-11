#lang racket

; just pile them up to build stacks, forgive me
(define original-stacks
  (hash-set*
   (hash-set*
    (hash-set*
     (hash-set*
      (hash-set*
       (hash-set*
        (hash-set*
         (hash-set*
          (hash-set*
           (make-immutable-hash)
           1 (list #\W #\R #\F))
          2 (list #\T #\H #\M #\C #\D #\V #\W #\P))
         3 (list #\P #\M #\Z #\N #\L))
        4 (list #\J #\C #\H #\R))
       5 (list #\C #\P #\G #\H #\Q #\T #\B))
      6 (list #\G #\C #\W #\L #\F #\Z))
     7 (list #\W #\V #\L #\Q #\Z #\J #\G #\C))
    8 (list #\P #\N #\R #\F #\W #\T #\V #\C))
   9 (list #\J #\W #\H #\G #\R #\S #\V)))


(define steps (with-input-from-file "steps.txt"
                (thunk
                 (sequence->list (in-lines)
                                 ))))

(define (parse-step step)
  (let ([result (string-split
                 (string-replace
                  (string-replace
                   (string-replace
                    (string-replace
                     step
                     "move" "")
                    "from" "")
                   "to" ""
                   )"  " " ") " ")]) (map string->number result)))

(define (get-stacks stacks reverse-order)
  (for/fold
   ([stacks stacks])
   ([step (map parse-step steps)])
    (letrec ([taken (take-right (hash-ref stacks (second step)) (first step))]
             [from-stack-after (drop-right (hash-ref stacks (second step)) (first step))]
             [to-stack-after (append
                              (hash-ref stacks (third step))
                              (if reverse-order (reverse taken) taken))])
      (values (hash-set*
               (hash-set* stacks (second step) from-stack-after) (third step) to-stack-after)))))

(define part-one-answers
  (list->string
   (for/fold
    ([result '()])
    ([i '(1 2 3 4 5 6 7 8 9)])
     (values (append result (list (last (hash-ref (get-stacks original-stacks #t) i))))))))

(display part-one-answers)
(display "\n")

(define part-two-answers
  (list->string
   (for/fold
    ([result '()])
    ([i '(1 2 3 4 5 6 7 8 9)])
     (values (append result (list (last (hash-ref (get-stacks original-stacks #f) i))))))))

(display part-two-answers)
(display "\n")
