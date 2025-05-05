(defpackage :turing-machines
  (:use :cl)
  (:export :main :run-machine))

(in-package :turing-machines)

(defstruct tape
  left     ;; list 
  current  ;; symbol under head
  right)   ;; list

(defun print-tape (tape)
  (format t "~%... ~a [~a] ~a ..."
          (reverse (tape-left tape))
          (tape-current tape)
          (tape-right tape)))

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

(defun move-right (tape)
  (if (null (tape-right tape))
      ;; If nothing to the right: assume blank '_'
      (make-tape :left (cons (tape-current tape) (tape-left tape))
                 :current '_
                 :right '())
      ;; Otherwise: move head right
      (make-tape :left (cons (tape-current tape) (tape-left tape))
                 :current (first (tape-right tape))
                 :right (rest (tape-right tape)))))

;; Key: (current-state current-symbol)
;; Value: (new-symbol direction next-state)
;; ((current-state symbol) (new-symbol direction next-state))
(defparameter *transitions*
  '(((start 1)   (0 right start))
    ((loop 0)    (1 left  start))
    ((start 0)   (0 right accept))
    ((loop 1)    (1 right accept))))

(defun process-step (state tape)
  (let* ((symbol (tape-current tape))
         (rule (assoc (list state symbol) *transitions* :test #'equal)))
    (if rule
        (destructuring-bind (new-symbol direction next-state) (second rule)
          ;; Construct a new tape with the current symbol changed
          (let ((updated-tape (make-tape
                                :left (tape-left tape)
                                :current new-symbol
                                :right (tape-right tape))))
            ;; Move based on direction
            (let ((moved-tape
                    (cond ((eq direction 'left)  (move-left updated-tape))
                          ((eq direction 'right) (move-right updated-tape))
                          (t updated-tape))))
              (values next-state moved-tape))))
        ;; No rule found: halt
        (values nil tape))))

(defun run-machine (state tape)
  (loop 
    do (format t "~%State: ~a" state)
    (print-tape tape)
    (multiple-value-bind (next-state new-tape) (process-step state tape)
    (if next-state
      (progn (setf state next-state)
              (setf tape new-tape))
      (return (values state tape))))))


(defun main ()
  (let ((initial-tape (make-tape :left '(0 0) :current 1 :right '(0 1 0 1 0))))
    (multiple-value-bind (final-state final-tape) (run-machine 'start initial-tape)
      (format t "~%Final state: ~a" final-state)
      (print-tape final-tape))))