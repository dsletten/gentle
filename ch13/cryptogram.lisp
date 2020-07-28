;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               cryptogram.lisp
;;;
;;;   STARTED:            Sat Jul 27 02:11:59 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

(let ((encipher-table (make-hash-table))
      (decipher-table (make-hash-table)))

  (defun make-substitution (c1 c2)
    (setf (gethash c1 decipher-table) c2
	  (gethash c2 encipher-table) c1))

  (defun undo-substitution (c)
    (let ((c1 (gethash c decipher-table)))
      (when c1
	(setf (gethash c decipher-table) nil
	      (gethash c1 encipher-table) nil))))

  (defun clear ()
    (clrhash decipher-table)
    (clrhash encipher-table))

  ;;;
  ;;;    We handle upper-case chars and punctuation too.
  ;;;    Undeciphered chars are returned as #\Space chars.
  ;;;    
  (defun decipher-string (s)
    (map 'string
	 #'(lambda (c)
	     (if (alpha-char-p c)
		 (let ((c1 (gethash (char-downcase c) decipher-table)))
		   (cond ((null c1) #\Space)
			 ((upper-case-p c) (char-upcase c1))
			 (t c1)))
		 c))
	 s))

  (defun show-line (s)
    (format t "~&~A~%" s)
    (format t "~A~%" (decipher-string s)))

  (defun show-text (l)
    (dolist (s l)
      (show-line s)
      (format t "~%")))

  (defun get-first-char (obj)
    (char-downcase (char (format nil "~A" obj) 0)))

  (defun read-letter ()
    (let ((input (read)))
      (case input
	((end undo) input)
	(t (get-first-char input)))) )

  (defun sub-letter (c)
    (cond ((gethash c decipher-table)
	   (format t "'~A' has already been deciphered as '~A'!~%"
		   c (gethash c decipher-table)))
	  (t (format t "What does '~A' decipher to? " c)
	     (let ((c1 (read-letter)))
	       (when (characterp c1)    ;Otherwise????
		 (if (gethash c1 encipher-table)
		     (format t "But '~A' already deciphers to '~A'!~%"
			     (gethash c1 encipher-table)
			     c1)
		     (make-substitution c c1)))) )))

  (defun undo-letter ()
    (format t "Undo which letter? ")
    (let ((c (read-letter)))
      (when (characterp c)
	(cond ((gethash c decipher-table)
	       (undo-substitution c))
	      (t (format t "'~A' has not been deciphered yet!~%" c)))) ))

  (defun solvedp (cryptogram)
    (every #'(lambda (c)
		      (if (alpha-char-p c)
			  (gethash (char-downcase c) decipher-table)
			  t))
	   (coerce (apply #'concatenate 'string cryptogram) 'list)))
  
  (defun solve (cryptogram)
    (do ((letter))
	((eq letter 'end) t)
      (when (solvedp cryptogram)
	(format t "~%Congratulations! You solved it.~%")
	(dolist (s cryptogram)
	  (format t "~A~%" (decipher-string s)))
	(return))
      (show-text cryptogram)
      (format t "Substitute which letter? ")
      (setf letter (read-letter))
      (cond ((eq letter 'undo) (undo-letter))
	    ((characterp letter) (sub-letter letter))
	    ((not (eq letter 'end)) (format t "Invalid input!~%")))) ) ;????
	    
	


  )

;"GEEGSJUMDQ, WTFMYSQ SJEJGMF TMFJDN. FY GF YN" "MYAYSSYV, VJ'DD FMGSM GTSTUL SJSPUF YN MWJ" "UTLWMDQ UJVF."
;APPARENTLY, HISTORY REPEATS ITSELF. SO AS OF
;TOMORROW, WE'LL START AIRING RERUNS OF THE
;NIGHTLY NEWS.
;"m cef tj e negb pbzbjgih, ejn gub vqh gttr xb utpfbsezr" "pmnmjv. gueg cef rmjn tw wqj, qjgmi cb pej tqg tw" "kqepgbpf.--fqfmb itqzrf"
; i was on a date recently, and the guy took me horseback
; riding. that was kind of fun, until we ran out of
; quarters.--susie loucks
;"tfetcf ols gev d rffc oqeyk pfkkdip ecz. d" "kfcc kgfn d goxf kgf lonf hyflkdei. d'n" "cfowidip ol d pe." "(toyc ldnei)"
; people ask how i feel about getting old. i
; tell them i have the same question. i'm
; learning as i go.
; (paul simon)
;"d ylguexm rdq wmuumx wm fjxur d urjetdaq fjxqt." "dzumx dvv, lu udcmt ey dvnjtu uflgm urm dnjeau" "jz qltc tydgm."
; a picture had better be worth a thousand words.
; after all, it takes up almost twice the amount
; of disk space.
;"tujivem va aveilovzjqx tqosobvftujw va mgexjowb" "fewoijotm mievqxbs mgxxtmi etzvhjqx ilt" "ivzt'm wfftqujc"
; editors of forthcoming encyclopedia of surgical
; practices strongly suggest removing the
; tome's appendix
