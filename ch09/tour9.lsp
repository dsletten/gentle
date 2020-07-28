;#!/usr/local/bin/clisp

;;
;   NAME:               tour9.lsp
;
;   STARTED:            010512
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;
;;;
;;;    9.1
;;;
(defun pilots ()
  (format t "There are old pilots,~%")
  (format t "and there are bold pilots,~%")
  (format t "but there are no old bold pilots.~%") )

(defun pilots-1 ()
  (format t (concatenate 'string 
			 "There are old pilots,~%"
			 "and there are bold pilots,~%"
			 "but there are no old bold pilots.~%")) )

;;;
;;;    9.2
;;;
(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
	(t (format t "*")
	   (draw-line (- n 1)))) )

;;;
;;;    9.3
;;;
(defun draw-box (cols rows)
  (cond ((zerop rows) nil)
	(t (draw-line cols)
	   (draw-box cols (- rows 1)))) )

;;;
;;;    9.4
;;;
(defun ninety-nine-bottles (beer)
  (cond ((zerop beer) (format t "Urrrp! No more beer, dude...~%"))
	(t (format t (concatenate 'string
				  "~D bottle~:P of beer on the wall,~%"   ;Handle pluralization properly: '~:P'
				  "~D bottle~:P of beer!~%"
				  "Take one down,~%"
				  "Pass it around,~%"
				  "~D bottle~:P of beer on the wall.~%~%")
		   beer beer (- beer 1))
	   (ninety-nine-bottles (1- beer)))) )

;;;
;;;    9.5
;;;    (Mightily recursive!)
;;;    (No need to unit test the subfunctions--they're simple enough?)
;;;    (Just like you don't test each and every line of code...)
(defun print-board (board-list)
  (labels ((col-1 (board-list)
	     (cond ((null board-list) nil)
		   (t (format t " ~A |" (cell-symbol (first board-list)))
		      (col-2 (rest board-list)))) )

	   (col-2 (board-list)
	     (format t " ~A |" (cell-symbol (first board-list)))
	     (col-3 (rest board-list)))

	   (col-3 (board-list)
	     (format t " ~A ~%" (cell-symbol (first board-list)))
	     (cond ((rest board-list) (format t "-----------~%")))              ;Not the final cell.
	     (col-1 (rest board-list)) )

	   (cell-symbol (sym)
	     (cond ((null sym) " ")
		   (t sym)) ))

    (col-1 board-list)) ) 



(defun write-to-file ()
  (with-open-file (stream "test.out" :direction :output :if-exists :new-version)
    (format stream "~S" (read))) )


; (defun copy-file (old new)
;   (let ((in-stream (open old))
; 	(out-stream (open new :direction :output :if-exists :supersede)))
; ;    (write-line (read-line in-stream nil nil) out-stream)
;     (do ((in-line (read-line in-stream nil nil) (read-line in-stream nil nil)))
; 	((not in-line))
;       (write-line in-line out-stream))

; ;((not (print (read-line in-stream nil nil)))))
; (close in-stream)
; (close out-stream)) )

; ;;
; ;;    Cleaned up version of above.
; ;;
; (defun copy-file (old new)
;   (let ((in-stream (open old))
; 	(out-stream (open new :direction :output :if-exists :supersede)))
;     (do ((in-line (read-line in-stream nil nil) (read-line in-stream nil nil)))
; 	((not in-line))
;       (write-line in-line out-stream))
;     (close in-stream)
;     (close out-stream)) )


(defun copy-file (old new)
  (with-open-file (in-stream old)
    (with-open-file (out-stream new :direction :output :if-exists :supersede)
      (do ((in-line (read-line in-stream nil nil)
		    (read-line in-stream nil nil)))
	  ((not in-line))
	(write-line in-line out-stream)))) )

;;;
;;;    9.10
;;;
(defun auto-test ()
  (test-space-over)
  (test-plot-one-point)
  (test-plot-points)
  (test-generate) )

(defun space-over (n)
  (labels ((space-over-aux (n)
	     (cond ((zerop n) nil)
		   (t (format t " ")
		      (space-over-aux (1- n)))) ))
    (cond ((< n 0) (format t "Error!~%"))
	  (t (space-over-aux n)))) )

;;
;;    Touretzky:
;;
; (defun space-over (n)
;   (cond ((plusp n) (format t " ") (space-over (- n 1)))
; 	((zerop n) nil)
; 	(t (format t "Error!"))) )

(defun test-space-over ()
  (let ((pos-arg 5)
	(neg-arg -5))
    (format t "Test space-over: positive arg ~A~%" pos-arg)
    (format t ">>>")
    (space-over pos-arg)
    (format t "<<<~%")

    (format t "Test space-over: negative arg ~A~%" neg-arg)
    (format t ">>>")
    (space-over neg-arg)
    (format t "<<<~%")) )

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string) )

(defun test-plot-one-point ()
  (let ((plot-sym "*"))
    (format t "Test plot-one-point: plotting-string ~S y-val (0 to 10)~%" plot-sym)
    (do ((n 0 (1+ n)))
	((> n 10))
      (plot-one-point plot-sym n))) )

(defun plot-points (s l)
  (cond ((null l) nil)
	(t (plot-one-point s (first l))
	   (plot-points s (rest l)))) )

(defun test-plot-points ()
  (let ((plot-sym "~")
	(y-list '(36 25 16 9 4 1 0 1 4 9 16 25 36)))
    (format t "Test plot-points: symbol ~S values ~S~%" plot-sym y-list)
    (plot-points plot-sym y-list)) )

(defun generate (m n)
  (labels ((generate-aux (m n)
	     (cond ((= m n) (list n))
		   (t (cons m (generate-aux (1+ m) n)))) ))
    (cond ((> m n) (generate-aux n m))
	  (t (generate-aux m n)))) )

(defun test-generate ()
  (format t "Test generate: m == n~%")
  (format t "~A~%" (generate 3 3))
  (format t "Test generate: m < n~%")
  (format t "~A~%" (generate -3 5))
  (format t "Test generate: m > n~%")
  (format t "~A~%" (generate 12 6)) )

;;
;;    How do I test this guy? (Simulate input)
;;
(defun make-graph ()
  (let ((func)
	(start)
	(end)
	(plotting-string))
    (format t "Please enter a function: ")
    (setf func (read))
    (format t "Please enter start x value: ")
    (setf start (read))
    (format t "Please enter final x value: ")
    (setf end (read))
    (format t "Please enter a symbol for plotting: ")
    (setf plotting-string (read-line))

    (plot-points plotting-string (mapcar func (generate start end)))) )

;;
;;    Touretzky: (No assignment (no setf))
;;
; (defun make-graph ()
;   (let* ((func (prompt-for "Function to graph? "))
; 	 (start (prompt-for "Starting x value? "))
; 	 (end (prompt-for "Ending x value? "))
; 	 (plotting-string (prompt-for "Plotting string? ")))
;     (plot-points plotting-string (mapcar func (generate start end)))
;     t) )

; (defun prompt-for (prompt-string)
;   (format t "~A" prompt-string)
;   (read) )
	 
(defun square (x)
  (* x x) )

(defun cube (x)
  (+ 30 (* x x x)) )

(defun read-my-file ()
  (with-open-file (stream "tour9.lsp")
    (let ((contents (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S objects from the file." (length contents))
      contents)) )

(defun read-all-objects (stream eof-indicator)
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
	nil
        (cons result (read-all-objects stream eof-indicator)))) )


;;;
;;;    9.11
;;;
(defun dot-prin1 (l)
  (cond ((atom l) (format t "~S" l))
	(t (format t "(")
	   (dot-prin1 (first l))
	   (format t " . ")
	   (dot-prin1 (rest l))
	   (format t ")"))) )

;;;
;;;    9.15
;;;
;;;    (Multiply recursive--functions call each other (and themselves) back and
;;;    forth.)
;;;
(defun hybrid-prin1 (l)
  (cond ((atom l) (format t "~S" l))     ;Print atom 'as is'
	(t (format t "(")                ;Otherwise, start printing a list
					 ;'(...'
	   (hybrid-prin1 (first l))
	   (cdr-print (rest l)))) )

(defun cdr-print (l)
  (cond ((null l) (format t ")"))        ;cdr is nil => end of list ')'
	((atom l) (format t " . ~S)" l)) ;cdr is other atom => end of dotted
					 ;  list ' . x )'
	(t (format t " ")                ;Otherwise, print the rest of the
					 ;list.
	   (hybrid-prin1 (first l))
	   (cdr-print (rest l)))) )

;;;
;;;    Another version (011223)
;;;    (But the one above is better.)
;;;    (hybrid-prin1 '(a . ((b . nil) . nil))) -- Spaces messed up...
;;;    
(defun hybrid-prin1 (l)
  (format t "(")
  (cond ((atom (car l)) (format t "~S" (car l)))
	(t (hybrid-prin1 (car l))))
  (hybrid-prin1-cdr (cdr l)) )

(defun hybrid-prin1-cdr (l)
  (cond ((null l) (format t ")"))
	((atom l) (format t " . ~S)" l))
	((atom (car l))
	 (format t " ~S" (car l))
	 (hybrid-prin1-cdr (cdr l)))
	(t (hybrid-prin1 (car l))
	   (hybrid-prin1-cdr (cdr l)))) )
			 
	 
;;
;;    Touretzky's
;;
; (defun hybrid-prin1 (x)
;   (cond ((atom x) (format t "~S" x))
; 	(t (hybrid-print-car (car x))
; 	   (hybrid-print-cdr (cdr x)))) )

; (defun hybrid-print-car (x)
;   (format t "(")
;   (hybrid-prin1 x) )

; (defun hybrid-print-cdr (x)
;   (cond ((null x) (format t ")"))
; 	((atom x) (format t " . ~S)" x))
; 	(t (format t " ")
; 	   (hybrid-prin1 (car x))
; 	   (hybrid-print-cdr (cdr x)))) )
