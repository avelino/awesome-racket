#lang info
(define version "0.1")
(define collection 'multi)
(define deps '(["base" #:version "6.1"]
               "html-lib"
               ["markdown" #:version "0.25"]
               "racket-index"
               "web-server-lib"
	       "2htdp"))
(define build-deps '("at-exp-lib"
                     "net-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "web-server-doc"
		     "2htdp"))
