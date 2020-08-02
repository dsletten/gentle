;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               touretzky-sdraw.lisp
;;;;
;;;;   Started:            Thu Jul 30 04:11:20 2020
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
;;;;   Notes: (use-package :touretzky-sdraw)
;;;;
;;;;

(defpackage :touretzky-sdraw 
  (:use :common-lisp)
  (:export :sdraw :sdraw-loop :scrawl))

(in-package :touretzky-sdraw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; The parameters below are in units of characters (horizontal) 
;;; and lines (vertical). They apply to all versions of SDRAW, 
;;; but their values may change if cons cells are being drawn as 
;;; bit maps rather than as character sequences.

(defparameter *sdraw-display-width* 79) 
;(defparameter *sdraw-horizontal-atom-cutoff* 79) 
;(defparameter *sdraw-horizontal-cons-cutoff* 65)

(defparameter *etc-string* "etc.") 
(defparameter *circ-string* "circ.") 
(defparameter *etc-spacing* 4) 
(defparameter *circ-spacing* 5)

(defparameter *inter-atom-h-spacing* 3) 
(defparameter *cons-atom-h-arrow-length* 9) 
(defparameter *inter-cons-v-arrow-length* 3) 
(defparameter *cons-v-arrow-offset-threshold* 2) 
(defparameter *cons-v-arrow-offset-value* 1)

(defparameter *sdraw-vertical-cutoff* 22) 
(defparameter *sdraw-num-lines* 25 "The maximum depth allocated for the display of an object. Essentially three times the number of nested levels allowed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW and subordinate definitions.

(defun sdraw (obj &key
                    (display-width *sdraw-display-width*)
                    (horizontal-atom-cutoff display-width)
                    (horizontal-cons-cutoff (- display-width 14))
                    (num-lines *sdraw-num-lines*))
  (declare (special display-width horizontal-atom-cutoff horizontal-cons-cutoff num-lines))
  (let ((line-endings (make-array num-lines)))
    (declare (special line-endings))
    (fill line-endings most-negative-fixnum) 
    (draw-structure (struct1 obj 0 0 nil)) 
    (values)))

(defun struct1 (obj row root-col obj-memory) 
  (cond ((atom obj)
         (struct-process-atom (format nil "~S" obj) row root-col)) 
        ((member obj obj-memory :test #'eq)
         (struct-process-circ row root-col))
        ((>= row *sdraw-vertical-cutoff*)
         (struct-process-etc row root-col))
        (t (struct-process-cons obj row root-col (cons obj obj-memory)))))

(defun struct-process-atom (atom-string row root-col) 
  (declare (special horizontal-atom-cutoff))
  (let* ((start-col (struct-find-start row root-col))
         (end-col (+ start-col (length atom-string)))) 
    (cond ((< end-col horizontal-atom-cutoff)
           (struct-record-position row end-col)
           `(atom ,row ,start-col ,atom-string))
          (t (struct-process-etc row root-col)))))

(defun struct-process-cons (obj row root-col obj-memory)
  (declare (special horizontal-cons-cutoff))
  (let* ((cons-start (struct-find-start row root-col))
         (car-structure (struct1 (car obj)
                                 (+ row *inter-cons-v-arrow-length*)
                                 cons-start obj-memory))
         (start-col (third car-structure)))
    (if (>= start-col horizontal-cons-cutoff)
        (struct-process-etc row root-col)
        `(cons ,row ,start-col ,car-structure
               ,(struct1 (cdr obj) row
                         (+ start-col *cons-atom-h-arrow-length*) 
                         obj-memory)))))

(defun struct-process-etc (row root-col)
  "Row is too long. Ellide ending."
  (let ((start-col (struct-find-start row root-col)))
    (struct-record-position 
     row
     (+ start-col (length *etc-string*) *etc-spacing*))
    `(msg ,row ,start-col ,*etc-string*)))

(defun struct-process-circ (row root-col)
  "Circular structure detected."
  (let ((start-col (struct-find-start row root-col)))
    (struct-record-position 
     row
     (+ start-col (length *circ-string*) *circ-spacing*))
    `(msg ,row ,start-col ,*circ-string*)))
 
(defun struct-find-start (row root-col)
  (declare (special line-endings))
  (max root-col (+ *inter-atom-h-spacing* (aref line-endings row))))

(defun struct-record-position (row end-col) 
  (declare (special line-endings))
  (setf (aref line-endings row) end-col))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW-LOOP and subordinate definitions.

(defparameter *sdraw-loop-prompt-string* "S> ")

(defun sdraw-loop ()
  "Read-eval-print loop using sdraw to display results."
  (format t "~&Type any Lisp expression, or (ABORT) to exit.~%~%") 
  (sdl1))

(defun sdl1 ()
  (loop
     (format t "~&~A" *sdraw-loop-prompt-string*)
     (let ((form (read)))
       (setf +++ ++ 
             ++ + 
             + -
             - form)
       (let ((result (multiple-value-list
                      (handler-case (eval form)
                        (error (condx) condx)))))
         (typecase (first result)
           (error (display-sdl-error result))
           (t (setf /// //
                    // /
                    / result
                    *** **
                    ** *
                    * (first result))
              (display-sdl-result *)))))))

(defun display-sdl-result (result) 
  (let* ((*print-circle* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-pretty* nil)
         (full-text (format nil "Result: ~S" result))
         (text (if (> (length full-text)
                      *sdraw-display-width*)
                   (concatenate 'string
                                 (subseq full-text 0 (- *sdraw-display-width* 4))
                                 "...)") 
                   full-text)))
    (sdraw result)
    (if (consp result)
        (format t "~%~A~%" text))
    (terpri)))

(defun display-sdl-error (error) 
  (format t "~A~%~%" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SCRAWL and subordinate definitions.

(defparameter *scrawl-prompt-string* "SCRAWL> ") 
(defvar *scrawl-object* nil)
(defvar *scrawl-current-obj*)
(defvar *extracting-sequence* nil)

(defun scrawl (obj)
  "Read-eval-print loop to travel through list"
  (format t "~&Crawl through list: 'H' for help, 'Q' to quit.~%~%") 
  (setf *scrawl-object* obj)
  (setf *scrawl-current-obj* obj)
  (setf *extracting-sequence* nil)
  (sdraw obj)
  (scrawl1))

(defun scrawl1 ()
  (loop
     (format t "~&~A" *scrawl-prompt-string*) 
     (let ((command (read-uppercase-char)))
       (case command
         (#\A (scrawl-car-cmd))
         (#\D (scrawl-cdr-cmd))
         (#\B (scrawl-back-up-cmd)) (#\S (scrawl-start-cmd))
         (#\H (display-scrawl-help)) (#\Q (return))
         (t (display-scrawl-error))))))

(defun scrawl-car-cmd ()
  (cond ((consp *scrawl-current-obj*)
         (push 'car *extracting-sequence*)
         (setf *scrawl-current-obj* (car *scrawl-current-obj*))) 
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%"))) 
  (display-scrawl-result))
 
(defun scrawl-cdr-cmd ()
  (cond ((consp *scrawl-current-obj*)
         (push 'cdr *extracting-sequence*)
         (setf *scrawl-current-obj* (cdr *scrawl-current-obj*))) 
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%"))) 
  (display-scrawl-result))

(defun scrawl-back-up-cmd () 
  (cond (*extracting-sequence*
         (pop *extracting-sequence*) 
         (setf *scrawl-current-obj*
               (extract-obj *extracting-sequence* *scrawl-object*))) 
        (t (format t "~&Already at beginning of object.")))
  (display-scrawl-result))

(defun scrawl-start-cmd ()
  (setf *scrawl-current-obj* *scrawl-object*)
  (setf *extracting-sequence* nil)
  (display-scrawl-result))

(defun extract-obj (seq obj) 
  (reduce #'funcall
            seq 
            :initial-value obj
            :from-end t))

(defun get-car/cdr-string ()
  (if (null *extracting-sequence*)
      (format nil "'~S" *scrawl-object*)
      (format nil "(c~Ar '~S)"
              (map 'string #'(lambda (x)
                               (ecase x
                                 (car #\a)
                                 (cdr #\d))) 
                    *extracting-sequence*)
              *scrawl-object*)))

(defun display-scrawl-result ()
  (let* ((*print-pretty* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-circle* t)
         (extract-string (get-car/cdr-string))
         (text (if (> (length extract-string) *sdraw-display-width*) 
                   (concatenate 'string
                                (subseq extract-string 0
                                        (- *sdraw-display-width* 4))
                                "...)") 
                   extract-string)))
    (sdraw *scrawl-current-obj*)
    (format t "~&~%~A~%~%" text)))

(defun display-scrawl-help ()
  (format t "~&Legal commands:  A)car   D)cdr  B)back up~%") 
  (format t "~&                 S)start Q)quit H)help~%"))

(defun display-scrawl-error ()
  (format t "~&Illegal command.~%") 
  (display-scrawl-help))

(defun read-uppercase-char () 
  (let ((response (read-line)))
    (and (plusp (length response))
         (char-upcase (char response 0)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following definitions are specific to the tty implementation.

(defparameter *cons-string* "[*|*]") 
(defparameter *cons-cell-flatsize* 5) 
(defparameter *cons-h-arrowshaft-char* #\-) 
(defparameter *cons-h-arrowhead-char* #\>) 
(defparameter *cons-v-line* "|") 
(defparameter *cons-v-arrowhead* "v")

(defun char-blt (row start-col string)
  "Clear out the existing text in ROW up to position START-COL."
  (declare (special textline-array textline-lengths))
  (let ((spos (aref textline-lengths row))
        (line (aref textline-array row))) 
    (fill line #\Space :start spos :end start-col)
    (replace line string :start1 start-col)
    (setf (aref textline-lengths row)
          (+ start-col (length string)))))

(defun draw-structure (directions) 
  (declare (special display-width num-lines))
  (let ((textline-array (make-array num-lines))
        (textline-lengths (make-array num-lines)))
    (declare (special textline-array textline-lengths))
    (dotimes (i (length textline-array))
      (setf (aref textline-array i) 
            (make-string display-width)))
    (fill textline-lengths 0) 
    (follow-directions directions) 
    (dump-display)))

(defun follow-directions (dirs &optional is-car) 
  (ecase (car dirs)
    (cons (draw-cons dirs))
    ((atom msg) (draw-msg dirs is-car))))

(defun draw-cons (directions)
  (declare (special textline-array textline-lengths))
  (destructuring-bind (type row col car-component cdr-component) directions
    (declare (ignore type))
    (let ((line (aref textline-array row))
          (h-arrow-start (+ col *cons-cell-flatsize*))
          (h-arrowhead-col (1- (third cdr-component))))
      (char-blt row col *cons-string*) 
      (do ((i h-arrow-start (1+ i)))
          ((>= i h-arrowhead-col))
        (setf (aref line i) *cons-h-arrowshaft-char*))
      (setf (aref line h-arrowhead-col) *cons-h-arrowhead-char*)
      (setf (aref textline-lengths row) (1+ h-arrowhead-col)) 
      (char-blt (+ row 1) (+ col 1) *cons-v-line*)
      (char-blt (+ row 2) (+ col 1) *cons-v-arrowhead*)
      (follow-directions car-component t)
      (follow-directions cdr-component))))

(defun draw-msg (directions is-car)
  (destructuring-bind (type row col string) directions
    (declare (ignore type))
  (char-blt row
            (+ col (if (and is-car (<= (length string) *cons-v-arrow-offset-threshold*)) 
                       *cons-v-arrow-offset-value*
                       0))
            string)))

(defun dump-display ()
  "Actually print the structure to the screen."
  (declare (special textline-array textline-lengths))
  (terpri)
  (loop for i from 0 below (length textline-array)
        for len = (aref textline-lengths i)
        for line = (aref textline-array i)
        while (plusp len)
        do (format t "~&~A" (subseq line 0 len)))
  (terpri))
