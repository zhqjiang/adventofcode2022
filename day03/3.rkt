#lang racket
(define compartments (with-input-from-file "3.txt"
                       (thunk
                        (sequence->list (in-lines)
                                        ))))

(define (find-common str1 str2)
  (first (set->list (set-intersect (list->set (string->list str1))
                                   (list->set (string->list str2))))))

(define (get-priority c)
  (if (char-lower-case? c)
      (+ (- (char->integer c) (char->integer #\a)) 1)
      (+ (- (char->integer c) (char->integer #\A)) 27)))

(define part-one-answer
  (apply + (map
            (lambda (str)
              (let ([left (substring str 0 (/ (string-length str) 2))]
                    [right (substring str (/ (string-length str) 2))])
                (get-priority (find-common left right))))
            compartments)))

(display part-one-answer )
(display "\n")

(define (chunk-by-three result l)
  (if (or (< (length l) 3))
      result
      (chunk-by-three (append result (list (take l 3))) (list-tail l 3))))

(define chunked (chunk-by-three '() compartments))
(define (get-items group)
  (first
   (set->list
    (apply set-intersect
           (map (lambda (rucksack) (list->set (string->list rucksack))) group)))))

(define part-two-answer
  (apply + (map
            (lambda (item) (get-priority item))
            (map (lambda (group) (get-items group)) chunked))))

(display part-two-answer )
(display "\n")