(defpackage #:gp.goof
  (:use :cl)
  (:documentation "Genetic programming is cool!"))

(in-package :gp.goof)

; Look! A Y Combinator.
(defun Y (r)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall r (lambda (x) (funcall (funcall f f) x))))))

(defun mutate (form)
  "Mutate takes a form and modifies the numbers randomly."
  (let ((sign (let ((x (random 2)))
                (if (= x 0) #'+ #'-))))
    (labels ((mutate-num (n)
               (cond
                 ((eq (class-of n) (find-class 'fixnum))
                  (funcall sign
                           (let ((x (random 5)))
                             (if (and (eq (first form) '/) (= x 0)) 1 x))
                           n))
                 ((eq (class-of n) (find-class 'cons))
                  (mapcar #'mutate-num n))
                 (t n))))
      (mapcar #'mutate-num form))))

(defun randomfun ()
  (let ((arithmetic (let ((x (random 4)))
                      (cond
                        ((= x 0) '+)
                        ((= x 1) '-)
                        ((= x 2) '*)
                        ((= x 3) '/)))))
    (let ((n1 (random 5))
          (n2 (let ((x (random 5)))
                (if (and (eq arithmetic '/) (= x 0)) 1 x))))
      (list arithmetic n1 n2))))