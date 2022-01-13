#lang racket

(require
    rackunit
    markdown)

(define readmemd (parse-markdown (file->string "./README.md")))

(define (check-alphabetically ul-list)
    (let ((ul (list)))
        (for ([i ul-list])
            (if (and (list? i) (>= (length i) 2))
                (set! ul (append ul (list (list-ref (list-ref i 2 ) 2)))) #f))
        (check-eq? ul (sort ul string<?))))

(test-case "alphabetically"
    (for ([i readmemd])
        (if (equal? (list-ref i 0) 'ul)
            (void? (check-alphabetically i))
            #f)))
