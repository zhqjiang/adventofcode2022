#lang racket

(define strategies (map
                    (lambda (str) (string-split str " "))
                    (with-input-from-file "2.txt"
                      (thunk
                       (sequence->list (in-lines)
                                       )))))


(define part-one-answer
  (apply +
         (map
          (lambda
              (strategy)
            (match strategy
              [(list "A" "X") (+ 1 3)]
              [(list "A" "Y") (+ 2 6)]
              [(list "A" "Z") (+ 3 0)]
              [(list "B" "X") (+ 1 0)]
              [(list "B" "Y") (+ 2 3)]
              [(list "B" "Z") (+ 3 6)]
              [(list "C" "X") (+ 1 6)]
              [(list "C" "Y") (+ 2 0)]
              [(list "C" "Z") (+ 3 3)]
              ))
          strategies)))

(define part-two-answer
  (apply +
         (map
          (lambda (strategy)
            (match strategy
              [(list "A" "X") (+ 3 0)]
              [(list "A" "Y") (+ 1 3)]
              [(list "A" "Z") (+ 2 6)]
              [(list "B" "X") (+ 1 0)]
              [(list "B" "Y") (+ 2 3)]
              [(list "B" "Z") (+ 3 6)]
              [(list "C" "X") (+ 2 0)]
              [(list "C" "Y") (+ 3 3)]
              [(list "C" "Z") (+ 1 6)]
              ))
          strategies)))

(display part-one-answer)
(display "\n")
(display part-two-answer)
(display "\n")

; A rock 1
; B paper 2
; C Scissors 3
