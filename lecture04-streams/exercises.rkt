#lang racket
(provide (all-defined-out))

; Infinite stream containing all ones
(define ones
  (stream-cons 1 ones))

; Return the nth element of a stream, counting from 0.
;
; Examples:
;  (stream-nth 0 (stream 1 2 3)) => 1
;  (stream-nth 2 (stream 1 2 3)) => 3

(define (stream-nth n s)
  (if (zero? n) (stream-first s)
      (stream-nth (- n 1) (stream-rest s))))

; Create a new stream that is the result of applying the function f to the
; stream s.
;
; Examples:
;   (stream->list (stream-map (lambda (x) (+ x 1)) (stream 0 1 2 3 4 5))) => (1 2 3 4 5 6)

(define (stream-map f s)
  (if (stream-empty? s) s
      (stream-cons (f (stream-first s))
                   (stream-map f (stream-rest s)))))

; Create a new stream containing only the element of s satisfying the predicate
; pred.
;
; Examples:
;   (stream->list (stream-filter (lambda (x) (odd? x)) (stream 0 1 2 3 4 5))) => (1 3 5)

(define (stream-filter pred s)
  (cond [(stream-empty? s) s]
        [(pred (stream-first s)) (stream-cons (stream-first s) (stream-filter pred (stream-rest s)))]
        [#t (stream-filter pred (stream-rest s))]))

; Create a stream containing all numbers lo, lo+1, lo+2, ..., hi
;
; Examples:
;  (stream-nth 100 (stream-enumerate 1 1000)) => 101
;
(define (stream-enumerate lo hi)
  (if (> lo hi) null
      (stream-cons lo (stream-enumerate (+ lo 1) hi))))

; Create a stream containing all numbers n, n+1, n+2, ...
; Note: this is an infinite stream
;
; Examples:
;  (stream-nth 100 (stream-enumerate-from 100)) => 200

(define (stream-enumerate-from n)
  (stream-cons n (stream-enumerate-from (+ n 1))))
