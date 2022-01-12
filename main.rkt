#lang racket

(require markdown
	racket/file
	2htdp/batch-io)

(define readmemd (parse-markdown (file->string "./README.md")))
(define markdown2html (xexpr->string `
	(html ()
		(head () (title "A curated list of awesome Racket frameworks, libraries and software, maintained by Community - Awesome Racket"))
		(body () ,@readmemd))))
(write-file "tmp/index.html" markdown2html)
