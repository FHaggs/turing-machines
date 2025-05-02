(defpackage :turing-machines
  (:use :cl)
  (:export :main))

(in-package :turing-machines)

(defstruct tape
  left     ;; list (reversed)
  current  ;; symbol under head
  right)   ;; list

(defun move-left (tape)
  (if (null (tape-left tape))
      ;; If nothing to the left: assume blank '_'
      (make-tape :left '()
                 :current '_
                 :right (cons (tape-current tape) (tape-right tape)))
      ;; Otherwise: move head left
      (make-tape :left (rest (tape-left tape))
                 :current (first (tape-left tape))
                 :right (cons (tape-current tape) (tape-right tape)))))



(defun main ()
  (setf tape (make-tape :left '(0 0) :current 1 :right '(0 1 0 1 0)))
  (print tape)
  (print (move-left tape)))
