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
;;;;   Notes: Extracted from the PDF file for the book. This is from 1989.
;;;;
;;;;

(defpackage :touretzky-sdraw (:use :common-lisp))

(in-package :touretzky-sdraw)

;;;    Package must exist before IN-PACKAGE!
;(in-package "SDRAW")

;;;    Weird quote char in PDF file...
;(export ’(touretzky-sdraw::sdraw touretzky-sdraw::sdraw-loop touretzky-sdraw::scrawl)) 
(export '(touretzky-sdraw::sdraw touretzky-sdraw::sdraw-loop touretzky-sdraw::scrawl)) 

;(shadowing-import ’(touretzky-sdraw::sdraw touretzky-sdraw::sdraw-loop touretzky-sdraw::scrawl)
;                   (find-package "USER"))

(shadowing-import '(touretzky-sdraw::sdraw touretzky-sdraw::sdraw-loop touretzky-sdraw::scrawl)
                   (find-package "CL-USER"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; The parameters below are in units of characters (horizontal) 
;;; and lines (vertical). They apply to all versions of SDRAW, 
;;; but their values may change if cons cells are being drawn as 
;;; bit maps rather than as character sequences.

(defparameter *sdraw-display-width* 79.) 
(defparameter *sdraw-horizontal-atom-cutoff* 79.) 
(defparameter *sdraw-horizontal-cons-cutoff* 65.)

(defparameter *etc-string* "etc.") 
(defparameter *circ-string* "circ.") 
(defparameter *etc-spacing* 4.) 
(defparameter *circ-spacing* 5.)

(defparameter *inter-atom-h-spacing* 3.) 
(defparameter *cons-atom-h-arrow-length* 9.) 
(defparameter *inter-cons-v-arrow-length* 3.) 
(defparameter *cons-v-arrow-offset-threshold* 2.) 
(defparameter *cons-v-arrow-offset-value* 1.)

(defparameter *sdraw-vertical-cutoff* 22.) 
(defparameter *sdraw-num-lines* 25)
(defvar *line-endings* (make-array *sdraw-num-lines*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW and subordinate definitions.

(defun sdraw (obj)
  (fill *line-endings* most-negative-fixnum) 
  (draw-structure (struct1 obj 0 0 nil)) 
  (values))

(defun struct1 (obj row root-col obj-memory) 
  (cond ((atom obj)
         (struct-process-atom (format nil "~S" obj) row root-col)) 
        ((member obj obj-memory :test #'eq)
         (struct-process-circ row root-col))
        ((>= row *sdraw-vertical-cutoff*)
         (struct-process-etc row root-col))
        (t (struct-process-cons obj row root-col
                                (cons obj obj-memory)))))

(defun struct-process-atom (atom-string row root-col) 
  (let* ((start-col (struct-find-start row root-col))
         (end-col (+ start-col (length atom-string)))) 
    (cond ((< end-col *sdraw-horizontal-atom-cutoff*)
           (struct-record-position row end-col)
           (list 'atom row start-col atom-string))
          (t (struct-process-etc row root-col)))))

(defun struct-process-etc (row root-col)
  (let ((start-col (struct-find-start row root-col)))
    (struct-record-position 
     row
     (+ start-col (length *etc-string*) *etc-spacing*))
    (list 'msg row start-col *etc-string*)))

(defun struct-process-circ (row root-col)
  (let ((start-col (struct-find-start row root-col)))
    (struct-record-position 
     row
     (+ start-col (length *circ-string*) *circ-spacing*))
    (list 'msg row start-col *circ-string*)))
 
(defun struct-process-cons (obj row root-col obj-memory)
  (let* ((cons-start (struct-find-start row root-col))
         (car-structure 
          (struct1 (car obj)
                   (+ row *inter-cons-v-arrow-length*)
                   cons-start obj-memory))
         (start-col (third car-structure)))
    (if (>= start-col *sdraw-horizontal-cons-cutoff*)
        (struct-process-etc row root-col)
        (list 'cons row start-col car-structure
               (struct1 (cdr obj) row
                        (+ start-col *cons-atom-h-arrow-length*) 
                        obj-memory)))))

(defun struct-find-start (row root-col)
  (max root-col (+ *inter-atom-h-spacing*
                   (aref *line-endings* row))))

(defun struct-record-position (row end-col) 
  (setf (aref *line-endings* row) end-col))

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
         (#\B (scrawl-back-up-cmd)) 
         (#\S (scrawl-start-cmd))
         (#\H (display-scrawl-help))
         (#\Q (return))
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

(defun display-scrawl-result (&aux (*print-pretty* nil)
                                (*print-length* nil)
                                (*print-level* nil)
                                (*print-circle* t)) 
  (let* ((extract-string (get-car/cdr-string))
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
(defparameter *cons-cell-flatsize* 5.) 
(defparameter *cons-h-arrowshaft-char* #\-) 
(defparameter *cons-h-arrowhead-char* #\>) 
(defparameter *cons-v-line* "|") 
(defparameter *cons-v-arrowhead* "v")

(defvar *textline-array* (make-array *sdraw-num-lines*)) 
(defvar *textline-lengths* (make-array *sdraw-num-lines*))

;(eval-when (eval load)
(eval-when (:execute :load-toplevel)
  (dotimes (i *sdraw-num-lines*)
    (setf (aref *textline-array* i) 
	  (make-string *sdraw-display-width*))))
          ;; (make-array *sdraw-display-width*
          ;;             :element-type 'string-char))))

(defun char-blt (row start-col string)
  (let ((spos (aref *textline-lengths* row))
        (line (aref *textline-array* row))) 
    (do ((i spos (1+ i)))
        ((>= i start-col))
      (setf (aref line i) #\Space))
    (replace line string :start1 start-col)
    (setf (aref *textline-lengths* row)
          (+ start-col (length string)))))

(defun draw-structure (directions) 
  (fill *textline-lengths* 0.) 
  (follow-directions directions) 
  (dump-display))

(defun follow-directions (dirs &optional is-car) 
  (ecase (car dirs)
    (cons (draw-cons dirs))
    ((atom msg) (draw-msg (second dirs)
                          (third dirs)
                          (fourth dirs)
                          is-car))))
 
(defun draw-cons (obj)
  (let* ((row (second obj))
         (col (third obj))
         (car-component (fourth obj))
         (cdr-component (fifth obj))
         (line (aref *textline-array* row))
         (h-arrow-start (+ col *cons-cell-flatsize*))
         (h-arrowhead-col (1- (third cdr-component))))
    (char-blt row col *cons-string*) 
    (do ((i h-arrow-start (1+ i)))
        ((>= i h-arrowhead-col))
      (setf (aref line i) *cons-h-arrowshaft-char*))
    (setf (aref line h-arrowhead-col) *cons-h-arrowhead-char*)
    (setf (aref *textline-lengths* row) (1+ h-arrowhead-col)) 
    (char-blt (+ row 1) (+ col 1) *cons-v-line*)
    (char-blt (+ row 2) (+ col 1) *cons-v-arrowhead*)
    (follow-directions car-component t)
    (follow-directions cdr-component)))

(defun draw-msg (row col string is-car)
  (char-blt row
            (+ col (if (and is-car
                            (<= (length string)
                                *cons-v-arrow-offset-threshold*)) 
                       *cons-v-arrow-offset-value*
                       0))
            string))

(defun dump-display ()
  (terpri)
  (dotimes (i *sdraw-num-lines*)
    (let ((len (aref *textline-lengths* i))) 
      (if (plusp len)
          (format t "~&~A"
                  (subseq (aref *textline-array* i) 0 len))
          (return nil))))
  (terpri))



