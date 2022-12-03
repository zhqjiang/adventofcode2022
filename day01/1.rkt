#lang racket
(define calories (with-input-from-file "1.txt"
                   (thunk
                    (sequence->list (in-lines)
                                    ))))

(define (run v1 v2 v3)
  (if (empty? v3)
      (if (number? v2)
          (run (append v1 (list v2)) '() v3)
          v1)
      (if (number? v2)
          (if (non-empty-string? (car v3))
              (run v1 (+ v2 (string->number (car v3))) (cdr v3))
              (run (append v1 (list v2)) '() (cdr v3)))
          (if (non-empty-string? (car v3))
              (run v1 (string->number (car v3)) (cdr v3))
              (run v1 '() (cdr v3))))))

(define calories-sum (run '() '() calories))
(define part-one-answer (apply max calories-sum))
(define part-two-answer
  (let ([sorted (sort calories-sum >)])
    (+ (first sorted) (second sorted) (third sorted))))

(display part-one-answer)
(display "\n")
(display part-two-answer)
(display "\n")

