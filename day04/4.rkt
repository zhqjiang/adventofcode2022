#lang racket
(define sections (with-input-from-file "4.txt"
                   (thunk
                    (sequence->list (in-lines)
                                    ))))

(define (judge-fully-contained? section-str)
  (letrec
      (
       [pairs (string-split section-str ",")]
       [first-pair (string-split (first pairs) "-")]
       [second-pair (string-split (second pairs) "-")]
       [first-start (string->number (first first-pair))]
       [first-end (string->number (second first-pair))]
       [second-start (string->number (first second-pair))]
       [second-end (string->number (second second-pair))]
       ) (or
          (and (>= first-start second-start) (<= first-end second-end))
          (and (<= first-start second-start) (>= first-end second-end)))))

(define part-one-answer
  (foldl
   (lambda
       (section result)
     (+ result (if (judge-fully-contained? section) 1 0)))
   0
   sections ) )

(display part-one-answer)
(display "\n")

(define (judge-overlapped? section-str)
  (letrec
      (
       [pairs (string-split section-str ",")]
       [first-pair (string-split (first pairs) "-")]
       [second-pair (string-split (second pairs) "-")]
       [first-start (string->number (first first-pair))]
       [first-end (string->number (second first-pair))]
       [second-start (string->number (first second-pair))]
       [second-end (string->number (second second-pair))]
       ) (or
          (and (>= second-start first-start) (<= second-start first-end))
          (and (>= first-start second-start) (<= first-start second-end)))))



(define part-two-answer
  (foldl
   (lambda
       (section result)
     (+ result (if (judge-overlapped? section) 1 0)))
   0
   sections ) )

(display part-two-answer)
(display "\n")

