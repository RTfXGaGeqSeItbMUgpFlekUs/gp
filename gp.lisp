(defpackage #:gp.goof
  (:use :cl)
  (:documentation "Genetic programming is cool!"))

(in-package :gp.goof)

(defun random-number-with-offset (sign offset)
  (let ((x (random 5)))
    (if (= (funcall (symbol-function sign) x offset) 0)
        (random-number-with-offset sign offset)
        x)))

(defun mutate (form)
  "Mutate takes a form and modifies the numbers randomly."
  (let ((sign (let ((x (random 2)))
                (if (= x 0) '+ '-))))
    (labels ((mutate-num (n)
               (cond
                 ((eq (class-of n) (find-class 'fixnum))
                  (funcall (symbol-function sign)
                           (random-number-with-offset sign n) n))
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
          (n2 (+ (random 4) 1)))
      (list arithmetic n1 n2))))