

(defun copy-stream (in out)
   (loop for line = (read-line in nil nil)
         while line
         do (write-line line out)))

(defun system (cmd)
  (with-open-stream (s1 (ext:run-program cmd :output :stream))
    (with-output-to-string (out)
      (copy-stream s1 out))))

(defun temp()
  (let ((temp (with-input-from-string (f (system "./temp.sh"))
			  (read f))))
    
    temp
    
    ))

(defun wait-for(pred)
  (loop :while (funcall pred) ))

(defun act(temp)
  (if (< temp 23.5)
      (progn
	(print (system "./relayOn.sh")) 
	(loop :while (< (temp) 23.562) do (format t "~%Cold:~A" (temp)) (sleep 3)))
    (progn
      (print (system "./relayOff.sh")) 
      (loop :while (>= (temp) 23.5) do (format t "~%Hot:~A" (temp)) (sleep 3)))
  ))
(loop do (act (temp)))
