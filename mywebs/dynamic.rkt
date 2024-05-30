#lang web-server/insta

(define (render-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))

(define (start request)
  (render-greeting "JiRen"))