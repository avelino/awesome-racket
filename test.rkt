#lang racket

(require
    rackunit
    markdown)

(define readmemd (parse-markdown (file->string "./README.md")))

(test-case "alphabetically summary"
    (let ((ul (list)))
        (for ([i (list-ref (list-ref (dict-ref readmemd 'ul) 1) 5)])
            (if (and (list? i) (>= (length i) 2))
                (set! ul (append ul (list (list-ref (list-ref i 2 ) 2)))) #f))
        (check-eq? ul (sort ul string<?))))
