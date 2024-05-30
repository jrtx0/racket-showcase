#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "component-lib"
                     "db-lib"
                     "koyo-lib"
                     "koyo-north"
                     "threading-lib"
                     "rackunit-lib"

                     "jrtx-studio"))

(define update-implies '("jrtx-studio"))
