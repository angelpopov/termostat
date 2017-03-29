(defun copy-stream (in out)
   (loop for line = (read-line in nil nil)
         while line
         do (write-line line out)))

(defun system (cmd args)
  (with-open-stream (s1 (ext:run-program cmd :arguments args :output :stream))
    (with-output-to-string (out)
      (copy-stream s1 out))))
(defparameter *temps* ())
(defun temp()
  (let ((temp (with-input-from-string (f (system "./temp.sh" '()))
				      (read f))))

    (push temp *temps*)
    (when (> (length *temps*) 0)
      (setf *temps* (subseq *temps* 0 1)))
    (let ((avg (/ (reduce #'+ *temps*) (length *temps*))))
      (format t "~%--~A ~A" avg temp)
      avg
    )
    ))

(defun action(n)
  (if (zerop n) (sleep 3)
    (system "./relay.sh" (list (format nil "~A" n)  )))
  (format t "~%action:~A" n))


(defun select(history)
  (if (null history) 0
    (let ((temp (rest(first history))))
      (when (< 0.0125 (abs(- temp 24.5)))
	(if (>= temp 24.5)
	    0
	  1)
	))))

(defun act(history)
  (let* ((action (select history)))
    (if action
	(act (list* (cons (action action) (temp)) history))
      history )))
(defparameter *goal* 28.5)

(defun get-temp(state)  (first (last (first state))))
(defun estimate(temps)
  (let((temp (get-temp temps)))
    (expt (abs (- (expt *goal* 2) (expt temp 2))) 1/2)))

(defun r(state action next-state)
  (+ (reduce #'+ (mapcar #'estimate (mapcar #'list state)))
     (estimate next-state)))

(defparameter *q* (make-hash-table :test #'equal))

(defun q(state action)
  (or (gethash (list state action) *q*)
      (let ((delta 0.3))
	(if (= action 0)
	    (estimate (list (list 't 0 (- (get-temp state) delta))))
	(estimate (list (list 't 100 (+ (get-temp state) delta)))))) ))


(defun actions(state)
  (cond
   ((>= (get-temp state) (+ *goal* 0.5)) (list 0))
   ((< (get-temp state) (- *goal* 0.5)) (list 100))
   (t (list 0 10 100))) )

(defparameter *history* (make-hash-table :test #'equal))
(defparameter *learning-rate* 0.01)
(defun new-Q(state action next-temp)
  (let*( (discounted-reward 0.85)
       (next-state (list* (list (get-temp state) action next-temp) (butlast state)))
       (history (gethash (list state action) *history*))
       (learning-rate (expt 0.5 (count-if #'null (mapcar #'> history (rest history)))))
       )
    ;(format t "~%New Q ~A action ~A -> ~A"state action next-state )
    (let ((res (+ (q state action)
		  (* learning-rate (-
			 (+ (r state action next-state)
			    (* discounted-reward 
			       (reduce
				#'max
				(mapcar
				 #'(lambda(action)(q next-state action))
				 (actions state)))))
			 (q state action))))))
      (push res (gethash (list state action) *history*))
      (format t "~%   ~A ~S" learning-rate (gethash (list state action) *history*))
      res)))

;;;; ; Martin Kersner, m.kersner@gmail.com
; 2016/03/01

; log of all elements in a list
(defun log_list (x)
  (mapcar #'log x))

; cross-entropy
; C = (-1/n)*sum[y*ln(a) + (1-y)*ln(1-a)]
(defun cross-entropy (a y)
  (let ((one_minus_a (mapcar #'1- a))
        (one_minus_y (mapcar #'1- y))
        (one_div_len (/ 1 (length a))))
  (- (* one_div_len (apply '+ (mapcar #'+ (mapcar #'* y (log_list a)) (mapcar #'* one_minus_y (log_list one_minus_a))))))))

;softmax function
; e^(x_i)/sum(x_i)
(defun softmax (x)
  (let* ((x_to_exp (mapcar #'exp x))
        (denominator (apply '+ x_to_exp)))
    (my-div x_to_exp denominator)))

(defun my-div (my-list denominator)
  (if my-list (progn 
                (cons (/ (car my-list) denominator)
                      (my-div (cdr my-list) denominator)))))

;;;;

(defun prepare-sample(a-q)
  (mapcan #'(lambda(a-q s)
	      (loop :repeat (* 1000 s) :collect (first a-q)))
	  a-q
	  (softmax (mapcar #'rest a-q))))

(defun counts(l) (mapcar #'(lambda(el)(cons el (count el l))) (remove-duplicates l)))

(defun qlearn(temp1)
  "temp->new-temp"
  ;(format t "~%Current:~A" temp)
  (let ((temp temp1))
    (when (>(get-temp temp) (- *goal* 0.5))
      (loop for x = (temp) :while (> x (- *goal* 0.5))
			:collect x do (sleep 10))
      
      (setf temp (loop :repeat (length temp)
		       :collect (list (temp) 0 (temp)))))
    (let* ((actions (prepare-sample (mapcar #'(lambda(a) (cons a (q temp a))) (actions temp))))
	   (action (nth (random (length actions)) actions)))
      (print (counts actions) )
      (action action)
      (sleep 3)
      (let ((new-temp (temp)))
	(setf (gethash (list temp action) *q*) (new-q temp action new-temp))
	(list* (list (get-temp temp) action new-temp) temp)))))

(defun reset-environment(&optional (temp (list (list (temp) 0 (temp))) ))
  "wait while temperature is bellow 27C"
  (if (>(get-temp temp) (- *goal* 0.005))
      (progn
	(loop for x = (temp) :while (> x (- *goal* 0.005))
	      :collect x do (sleep 10))
	(loop :repeat (length temp)
		       :collect (list (temp) 0 (temp))))
    temp))

(defun next-step(temp)
  (let* ((vals (mapcar #'(lambda(a) (cons a (q temp a))) (actions temp)))
					;(actions (prepare-sample vals))
	 (selected (first(reduce #'(lambda (min a) (if (< (rest a) (rest min)) a min))
				 vals)))
	 (actions (append (actions temp) (loop for x from 1 to 1000 :collect selected)))
	 (action (nth (random (length actions)) actions)))
    (print vals )
    (action action)
    (sleep 3)
    (let ((new-temp (temp)))
      (setf (gethash (list temp action) *q*) (new-q temp action new-temp))
      (butlast (list* (list (get-temp temp) action new-temp) temp)))))

(defun q-episode()
  (format t"~%Restart:~%")
  (loop for x from 1 to 6000
	:for temp = (reset-environment) 
	then (next-step temp)
	:if (>= (get-temp temp) (+ *goal* 1)) :return x
	do (format t " ~A " x) (sleep 3)))
(let((episodes '()))
  (loop :repeat 1000 do (push (q-episode) episodes) (print episodes)))

;(loop for x = (loop :repeat 4 :collect (list (temp) 0 (temp))) then (butlast (qlearn x)))

