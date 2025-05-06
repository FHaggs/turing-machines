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
      ;; If nothing to the left: assume blank 'blank'
      (make-tape :left '()
                 :current 'blank
                 :right (cons (tape-current tape) (tape-right tape)))
      ;; Otherwise: move head left
      (make-tape :left (rest (tape-left tape))
                 :current (first (tape-left tape))
                 :right (cons (tape-current tape) (tape-right tape)))))

(defun move-right (tape)
  (if (null (tape-right tape))
      ;; If nothing to the right: assume blank 'blank'
      (make-tape :left (cons (tape-current tape) (tape-left tape))
                 :current 'blank
                 :right '())
      ;; Otherwise: move head right
      (make-tape :left (cons (tape-current tape) (tape-left tape))
                 :current (first (tape-right tape))
                 :right (rest (tape-right tape)))))

;; Key: (current-state current-symbol)
;; Value: (new-symbol direction next-state)
;; ((current-state symbol) (new-symbol direction next-state))


(defun process-step (state tape transitions)
  (let* ((symbol (tape-current tape))
         (rule (gethash (list state symbol) transitions)))
    (if rule
        (destructuring-bind (new-symbol direction next-state) rule
          (let ((updated-tape (make-tape
                                :left (tape-left tape)
                                :current new-symbol
                                :right (tape-right tape))))
            (let ((moved-tape
                    (cond ((eq direction 'left)  (move-left updated-tape))
                          ((eq direction 'right) (move-right updated-tape))
                          (t updated-tape))))
              (values next-state moved-tape))))
        (values nil tape))))

(defun run-machine (initial-state initial-tape transitions)
  (loop with state = initial-state
        with tape = initial-tape
    do (format t "~%State: ~a" state)
    (print-tape tape)
    (multiple-value-bind (next-state new-tape) (process-step state tape transitions)
      (if next-state
          (progn (setf state next-state)
                 (setf tape new-tape))
          (return (values state tape))))))


(defmacro define-turing-machine (name &body transitions)
  `(defparameter ,(intern (format nil "*~a-TRANSITIONS*" (string-upcase name)))
     (let ((table (make-hash-table :test #'equal)))
       (dolist (transition ',transitions table)
         (destructuring-bind (current-state current-symbol next-symbol direction next-state) transition
           (setf (gethash (list current-state current-symbol) table)
                 (list next-symbol direction next-state)))))))



(define-turing-machine simple-adder
  (start 1 1 right start)
  (start 0 0 right start)
  (start blank blank left carry)
  (carry 0 1 right clean-up)
  (carry blank 1 right  clean-up)
  (carry 1 0 left carry)
  (clean-up 1 1 left clean-up)
  (clean-up 0 0 left clean-up)
  (clean-up blank blank right final))

(define-turing-machine another-machine
  (q1 a b right q2)
  (q2 b a left  q1)
  (q2 blank blank right halt))

(defun main ()
  (let ((initial-tape (make-tape :left '(0 0) :current 0 :right '(0 1 0 1 1))))
    (multiple-value-bind (final-state final-tape)
        (run-machine 'start initial-tape *simple-adder-transitions*)
      (format t "~%Final state (simple-adder): ~a" final-state)
      (print-tape final-tape))))