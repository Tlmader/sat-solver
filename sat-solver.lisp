;;; A global variable that keeps track of how many calls are made to #'satisfiable.
(defvar *calls* 0)

;;; Defines how many calls are allowed to #'satisfiable before an error is thrown.
(defvar *max-calls* 10)

(defmacro defun-limited (name args &body body)
 "Defines a function which can only be called *max-calls* number of times."
 `(defun ,name ,args
   (let ((result nil))
    (progn (setq *calls* (1+ *calls*))
     (when (> *calls* *max-calls*)
      (error "Call limit exceeded."))
     (setq result ,@body)
     (setq *calls* (1- *calls*))
     result))))

(defun grade (problem)
 "A function for grading an individual problem instance."
 (let ((number (first problem))
       (expression (second problem))
       (correct-answer (third problem))
       (your-answer 'error))
  (unwind-protect
   (setq your-answer (satisfiable (second problem)))
   (format t "Expression ~S: ~S~%  Correct Answer: ~S~%  Your Answer:    ~S~%" number expression correct-answer your-answer))))

;;; ---------- BEGIN STUDENT CODE ----------
;;; Your code will be pasted in starting here.

;;; Finds a model which satisfies a CNF expression.
(defun satisfiable (cnf)
 (let ((simpl (simplify cnf))
       (result nil))
  (cond ((eq simpl t) t) ; If every clause is true, return true.
   ((eq simpl nil) nil)  ; If any clause is empty, return false.
   ((symbolp simpl) t)
   ((mapcar
     (lambda (var)
       ; Set V=T. Try to find a model that satisfies.
      (cond ((eq (satisfiable(set-variable simpl var t)) t)
             (setq result t))
       ; Set V=F. Try to find a model that satisfies.
       ((eq (satisfiable(set-variable simpl var nil)) t)
        (setq result t))))
     (remove 'and (remove 'or (remove 'not (remove-duplicates (flatten simpl))))))
    result)))) ; Return result.

;;; Feel free to define other functions here if you want.
;;; Simplifies a CNF formula
(defun simplify (cnf)
 (if (symbolp cnf)
  cnf
  (let ((simplified (mapcar #'simplify cnf)))
   (cond ((eq (first simplified) 'not)
          (cond ((eq (second simplified) t) nil)
           ((eq (second simplified) nil) t)
           (t simplified)))
    ((eq (first simplified) 'and)
     (let ((shorter (remove t simplified)))
      (cond ((equal shorter '(and)) t)
       ((member nil shorter) nil)
       (t shorter))))
    ((eq (first simplified) 'or)
     (let ((shorter (remove nil simplified)))
      (cond ((equal shorter '(or)) nil)
       ((member t shorter) t)
       ((eq 2 (length shorter)) (second shorter))
       (t shorter))))))))

;;; Substitutes a value in a CNF formula
(defun set-variable (cnf variable value)
 (if (symbolp cnf)
  (if (eq cnf variable) value cnf)
  (mapcar
   (lambda (part)
    (set-variable part variable value))
   cnf)))

(defun flatten (orig-lst)
 (if (eq orig-lst nil)
  nil
  (let ((elem (car orig-lst))
        (rest-of-lst (cdr orig-lst)))
   (if (listp elem)
    (append (flatten elem) (flatten rest-of-lst))
    (append (cons elem nil) (flatten rest-of-lst))))))

;;; ---------- END STUDENT CODE ----------

;;; Test the #'satisfiable function on various different CNF expressions.
;;; Some examples are provided here, but the actual problems that you will be
;;; graded on will be different from these.  However, if you can solve all of
;;; these correctly and (for those in CSCI 5525, without exceeding the call
;;; limit), then you will probably pass all the graded tests.
;;; The format of a test is: (number expression expected-value).
;;; For example, expression #6 is (and a b) and it is satisfiable, so the
;;; expected return value of #'satisfiable is t.
(mapcar #'grade '((1 t t)
                  (2 nil nil)
                  (3 a t)
                  (4 (and t) t)
                  (5 (and nil) nil)
                  (6 (and a b) t)
                  (7 (or t) t)
                  (8 (or nil) nil)
                  (9 (or a b) t)
                  (10 (and a (not a)) nil)
                  (11 (and a (or (not a) b)) t)
                  (12 (and a b (or (not a) (not b))) nil)
                  (13 (and (or a b c) (not b) (or (not a) c) (or a d) (not c)) nil)
                  (14 (and (not a) (not b) (not c) (not d) (not e) (not f) (not g) (not h) (not i) (not j) (or a b c d e f g h i j k)) t)))
