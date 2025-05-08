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
  "Defines a Turing machine with the given name and transitions.
  The inital state is the state of the first transition.
   Creates a function named RUN-NAME to run the machine."
  (let* ((sym-name (string-upcase (symbol-name name)))
         (run-fn (intern (format nil "RUN-~A" sym-name)))
         (initial-state (first (first transitions))))
    `(defun ,run-fn (tape)
        (let ((transitions-table (make-hash-table :test #'equal)))
          ,@(loop for (current-state current-symbol new-symbol direction new-state) in transitions
            collect `(setf (gethash (list ',current-state ',current-symbol) transitions-table)
              (list ',new-symbol ',direction ', new-state)))
              (run-machine ',initial-state tape transitions-table)))))


; (machine sym other-machine)
; (defmacro define-composable-machine (name &body transitions)
  ; (let* ((syn-name (string-upcase (symbol-name name)))
         ; (run-fn (intern (format nil "RUN-~A" syn-name)))
         ; (inital-machine (first (first transitions))))
    ; `(defun ,run-fn (tape)
       ; (let ((transitions-table (make-hash-table :test #'equal)))
         ; ,@(loop for (current-machine current-symbol next-machine) in transitions
                 ; collect `(setf (gethash (list ',current-machine ',current-symbol) transitions-table)
                                ; (#',next-machine)))
         ; (run-composed-machine #',inital-machine tape transitions-table)))))
;

(defvar *function-map* nil
  "A global hash table mapping (machine-symbol, input-symbol) pairs to machine-symbols.")

(defun initialize-function-map ()
  (setf *function-map* (make-hash-table :test #'equal)) ; 'equal' to match list keys
  (setf (gethash (list 'run-simple-adder 0) *function-map*) 'run-simple-adder)
  (setf (gethash (list 'run-simple-adder 1) *function-map*) 'run-another-machine))

(defun run-composed-machine (initial-machine-symbol tape transitions-table)
  ;; Run the machine and capture the resulting tape
  (multiple-value-bind (final-state new-tape)
      (funcall (symbol-function initial-machine-symbol) tape)
    (print final-state)
    (let* ((symbol (tape-current new-tape))
           (new-machine-symbol (gethash (list initial-machine-symbol symbol) transitions-table)))
      (when new-machine-symbol
        (run-composed-machine new-machine-symbol new-tape transitions-table)))))

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
  (q1 0 0 right halt)
  (q2 1 1 right  halt))

; (define-composable-machine big-m
  ; (run-simple-adder 0 run-simple-adder)
  ; (run-simple-adder 1 run-another-machine))
;
(defun main ()
  (initialize-function-map)
  (let ((initial-tape (make-tape :left '(0 0) :current 0 :right '(0 1 0 1 1))))
    (run-composed-machine 'run-simple-adder initial-tape *function-map*)))
(main)
