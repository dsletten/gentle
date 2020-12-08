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

(defun make-substitution (in out)
  (setf (gethash in *encipher-table*) out)
  (setf (gethash out *decipher-table*) in))

(defun undo-substitution (ch)
  (remhash (decode-char ch) *encipher-table*)
  (remhash ch *decipher-table*))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (encoded)
;  (let ((decoded (make-string (length encoded) :initial-element #\space)))
  (map 'string #'(lambda (ch) (gethash ch *decipher-table* #\space)) encoded))

(defun show-line (line)
  (format t "~A~%" line)
  (format t "~A~%" (decipher-string line)))

(defun show-text (text)
  (dolist (line text)
    (show-line line)))

(defun decode-char (ch)
  (gethash ch *decipher-table*))

(defun encode-char (ch)
  (gethash ch *encipher-table*))

(defun sub-letter (ch)
  (let ((ch1 (decode-char ch)))
    (if ch1
        (warn "'~C' has alreaday been deciphered as '~C'!" ch ch1)
        (let* ((ch1 (char-downcase (char (prompt-read (format nil "What does '~C' decipher to? " ch) :allow-empty nil) 0)))
               (ch0 (encode-char ch1)))
          (if ch0
              (warn "But '~C' already deciphers to '~C'!" ch0 ch1)
              (make-substitution ch1 ch)))) ))

(defun undo-letter ()
  (let* ((ch (char-downcase (char (prompt-read "Undo which letter?" :allow-empty nil) 0)))
         (ch1 (decode-char ch)))
    (if ch1
        (undo-substitution ch)
        (warn "'~C' has not been deciphered yet." ch))))

(defun solvedp (cryptogram)
  (etypecase cryptogram
    (list (every #'solvedp cryptogram))
    (string (dotimes (i (length cryptogram) t)
              (unless (or (char= (char cryptogram i) #\space)
                          (decode-char (char cryptogram i)))
                (return nil)))) ))

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


