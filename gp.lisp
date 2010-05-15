(defpackage #:gp.goof
  (:use :cl)
  (:documentation "Genetic programming is cool!")
  (:export #:mutate
           #:randomfun
           #:find-for-n))

(in-package :gp.goof)

(defun random-number-with-offset (&optional (sign '+) (offset 0))
  (let ((x (random 6)))
    (if (= (funcall (symbol-function sign) (- x 2) offset) 0)
        (random-number-with-offset sign offset)
        (- x 2))))

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
    (let ((n1 (random-number-with-offset))
          (n2 (random-number-with-offset)))
      (list arithmetic n1 n2))))

(defun find-for-n (n &key (print))
  "Find the function that returns n."
  (let ((curfun (randomfun)))
    (loop until (= (eval curfun) n) do
         (if print
             (format t "~a: ~a~%" curfun (eval curfun)))
         (setq curfun (mutate curfun)))
    curfun))
