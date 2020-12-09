;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               cryptogram.lisp
;;;;
;;;;   Started:            Sun Dec  6 22:22:35 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :cryptogram (:use :common-lisp :lang :test))

(in-package :cryptogram)

(defvar *crypto-text* (list "zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
                            "enlpo pib slafml pvv bfwkj"))

(defvar *encipher-table* (make-hash-table :test #'equalp))
(defvar *decipher-table* (make-hash-table :test #'equalp))

(defun make-substitution (clr enc)
  (setf (gethash clr *encipher-table*) enc
        (gethash enc *decipher-table*) clr))

(defun undo-substitution (clr enc)
  (remhash clr *encipher-table*)
  (remhash enc *decipher-table*))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (encoded)
;  (let ((decoded (make-string (length encoded) :initial-element #\space)))
  (map 'string 
       #'(lambda (enc) (if (alpha-char-p enc) (match-case enc (decode-char enc #\space)) enc)) 
       encoded))

(defun match-case (enc clr)
  (cond ((not (alpha-char-p clr)) clr)
        ((upper-case-p enc) (char-upcase clr))
        (t clr)))

(defun show-line (line)
  (format t "~A~%" line)
  (format t "~A~2%" (decipher-string line)))

(defun show-text (text)
  (dolist (line text)
    (show-line line)))

(defun decode-char (enc &optional default)
  (gethash enc *decipher-table* default))

(defun encode-char (clr)
  (gethash clr *encipher-table*))

(defun sub-letter (enc)
  (let ((clr (decode-char enc)))
    (if clr
        (warn "'~C' has alreaday been deciphered as '~C'!" enc clr)
        (let* ((clr (char-downcase (char (prompt-read (format nil "What does '~C' decipher to? " enc) :allow-empty nil) 0)))
               (enc* (encode-char clr)))
          (if enc*
              (warn "But '~C' already deciphers to '~C'!" enc* clr)
              (make-substitution clr enc)))) ))

(defun undo-letter ()
  (let* ((enc (char-downcase (char (prompt-read "Undo which letter? " :allow-empty nil) 0))) ; Check input!
         (clr (decode-char enc)))
    (if clr
        (undo-substitution clr enc)
        (warn "'~C' has not been deciphered yet." enc))))

;; (defun solvedp (cryptogram)
;;   (etypecase cryptogram
;;     (list (every #'solvedp cryptogram))
;;     (string (loop for ch across cryptogram
;;                   when (alpha-char-p ch)
;;                     unless (decode-char ch)
;;                       return nil
;;                     end
;;                   end
;;                   finally (return t)))) )

(defun solvedp (cryptogram)
  (etypecase cryptogram
    (character (if (alpha-char-p cryptogram) (decode-char cryptogram) t))
    (list (every #'solvedp cryptogram))
    (string (loop for ch across cryptogram
                  unless (solvedp ch)
                    return nil
                  finally (return t)))) )

(defun solve (cryptogram)
  (labels ((execute ()
             (show-text cryptogram)
             (cond ((solvedp cryptogram) :done)
                   (t (let ((command (prompt-read "Substitute which letter? " :allow-empty nil)))
                        (cond ((string-equal command "undo") (undo-letter))
                              ((and (= (length command) 1) (alpha-char-p (char command 0)))
                               (sub-letter (char command 0)))
                              (t (warn "Invalid input."))))
                      (execute)))) )
    (execute)))

;;;
;;;    See 2002 version for more examples.
;;;    
;"M cef tj e negb pbzbjgih, ejn gub vqh gttr xb utpfbsezr" "pmnmjv. gueg cef rmjn tw wqj, qjgmi cb pej tqg tw" "kqepgbpf." "--Fqfmb Itqzrf"
; i was on a date recently, and the guy took me horseback
; riding. that was kind of fun, until we ran out of
; quarters.--susie loucks
