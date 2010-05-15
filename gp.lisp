(defpackage #:gp.goof
  (:use :cl)
  (:documentation "Genetic programming is cool!"))

(in-package :gp.goof)

; Look! A Y Combinator.
(defun Y (r)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall r (lambda (x) (funcall (funcall f f) x))))))

; So apparently labels is better than Y, but I'm keeping the Y.
(defun mutate (form)
  "Mutate takes a form and modifies the numbers randomly."
  (let ((sign (let ((x (random 2)))
                (if (= x 0) #'+ #'-))))
    (mapcar (Y (lambda (fun)
                 (lambda (n)
                   (cond
                     ((eq (class-of n) (find-class 'fixnum))
                      (funcall sign (random 5) n))
                     ((eq (class-of n) (find-class 'cons))
                      (mapcar fun n))
                     (t n)))))
             form)))
