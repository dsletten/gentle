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

(defparameter *etc-string* "etc.") 
(defparameter *circ-string* "circ.") 
(defparameter *etc-spacing* 4) 
(defparameter *circ-spacing* 5)

(defparameter *inter-atom-h-spacing* 3) 
(defparameter *cons-atom-h-arrow-length* 9) 
(defparameter *inter-cons-v-arrow-length* 3) 
(defparameter *cons-v-arrow-offset-threshold* 2) 
(defparameter *cons-v-arrow-offset-value* 1)

(defparameter *sdraw-num-lines* 25 "The maximum depth allocated for the display of an object. Essentially three times the number of nested levels allowed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW and subordinate definitions.

(defclass renderer ()
  ((display-width :reader display-width :initform *sdraw-display-width* :initarg :display-width)
   (horizontal-atom-cutoff :reader horizontal-atom-cutoff :initarg :horizontal-atom-cutoff)
   (horizontal-cons-cutoff :reader horizontal-cons-cutoff :initarg :horizontal-cons-cutoff)
   (num-lines :reader num-lines :initform *sdraw-num-lines* :initarg :num-lines)
   (vertical-cutoff :reader vertical-cutoff)
   (line-endings :reader line-endings)))

(defmethod initialize-instance :after ((r renderer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (num-lines line-endings vertical-cutoff) r
    (setf line-endings (make-array num-lines :initial-element most-negative-fixnum)
          vertical-cutoff (- num-lines 3))))

(defgeneric draw-structure (renderer directions))

(defun sdraw (obj &key
                    (display-width *sdraw-display-width*)
                    (horizontal-atom-cutoff display-width)
                    (horizontal-cons-cutoff (- display-width 14))
                    (num-lines *sdraw-num-lines*))
  (let ((renderer (make-instance 'tty 
                                 :display-width display-width
                                 :horizontal-atom-cutoff horizontal-atom-cutoff
                                 :horizontal-cons-cutoff horizontal-cons-cutoff
                                 :num-lines num-lines)))
    (draw-structure renderer (struct1 renderer obj 0 0 nil)) 
    (values)))

(defun struct1 (renderer obj row root-col cache) 
  (cond ((atom obj)
         (struct-process-atom renderer (format nil "~S" obj) row root-col)) 
        ((member obj cache :test #'eq)
         (struct-process-circ renderer row root-col))
        ((>= row (vertical-cutoff renderer))
         (struct-process-etc renderer row root-col))
        (t (struct-process-cons renderer obj row root-col (cons obj cache)))) )

(defun struct-process-atom (renderer atom-string row root-col) 
  (let* ((start-col (struct-find-start renderer row root-col))
         (end-col (+ start-col (length atom-string)))) 
    (cond ((< end-col (horizontal-atom-cutoff renderer))
           (struct-record-position renderer row end-col)
           `(atom ,row ,start-col ,atom-string))
          (t (struct-process-etc renderer row root-col)))) )

(defun struct-process-cons (renderer obj row root-col cache)
  (let* ((cons-start (struct-find-start renderer row root-col))
         (car-structure (struct1 renderer
                                 (car obj)
                                 (+ row *inter-cons-v-arrow-length*)
                                 cons-start cache))
         (start-col (third car-structure)))
    (if (>= start-col (horizontal-cons-cutoff renderer))
        (struct-process-etc renderer row root-col)
        `(cons ,row ,start-col ,car-structure
               ,(struct1 renderer (cdr obj) row
                         (+ start-col *cons-atom-h-arrow-length*) 
                         cache)))) )

(defun struct-process-etc (renderer row root-col)
  "Row is too long. Ellide ending."
  (let ((start-col (struct-find-start renderer row root-col)))
    (struct-record-position renderer
                            row
                            (+ start-col (length *etc-string*) *etc-spacing*))
    `(msg ,row ,start-col ,*etc-string*)))

(defun struct-process-circ (renderer row root-col)
  "Circular structure detected."
  (let ((start-col (struct-find-start renderer row root-col)))
    (struct-record-position renderer
                            row
                            (+ start-col (length *circ-string*) *circ-spacing*))
    `(msg ,row ,start-col ,*circ-string*)))
 
(defun struct-find-start (renderer row root-col)
  (with-slots (line-endings) renderer
    (max root-col (+ *inter-atom-h-spacing* (aref line-endings row)))) )

(defun struct-record-position (renderer row end-col) 
  (with-slots (line-endings) renderer
    (setf (aref line-endings row) end-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW-LOOP and subordinate definitions.

(defparameter *sdraw-loop-prompt-string* "S> ")

(defun sdraw-loop (&key (display-width *sdraw-display-width*))
  "Read-eval-print loop using sdraw to display results."
  (declare (special display-width))
  (format t "~&Type any Lisp expression, or (ABORT) to exit.~%~%") 
  (sdl1))

(defun sdl1 ()
  (loop
     (format t "~&~A" *sdraw-loop-prompt-string*)
     (force-output)
     (let ((form (read)))
       (shiftf +++ ++ + - form)
       (let ((result (multiple-value-list
                      (handler-case (eval form)
                        (error (condx) condx)))))
         (typecase (first result)
           (error (display-sdl-error result))
           (t (shiftf /// // / result)
              (shiftf *** ** * (first result))
              (display-sdl-result *)))))))

(defun display-sdl-result (result) 
  (declare (special display-width))
  (let* ((*print-circle* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-pretty* nil)
         (full-text (format nil "Result: ~S" result))
         (text (if (> (length full-text) display-width)
                   (ellide full-text display-width)
                   full-text)))
    (sdraw result :display-width display-width)
    (when (consp result)
      (format t "~%~A~%" text))
    (terpri)))

(defun ellide (s width)
  (let ((ellipsis "...)"))
    (concatenate 'string (subseq s 0 (- width (length ellipsis))) ellipsis)))

(defun display-sdl-error (error) 
  (format t "~A~%~%" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SCRAWL and subordinate definitions.

(defparameter *scrawl-prompt-string* "SCRAWL> ") 

(defun scrawl (obj &key (display-width *sdraw-display-width*))
  "Read-eval-print loop to travel through list"
  (declare (special display-width))
  (format t "~&Crawl through list: 'H' for help, 'Q' to quit.~%~%") 
  (let ((scrawl-object obj)
        (scrawl-current-obj obj)
        (extracting-sequence nil))
    (declare (special scrawl-object scrawl-current-obj extracting-sequence))
    (sdraw obj :display-width display-width)
    (scrawl1)))

(defun scrawl1 ()
  (loop
     (format t "~&~A" *scrawl-prompt-string*) 
     (force-output)
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
  (declare (special scrawl-current-obj extracting-sequence))
  (cond ((consp scrawl-current-obj)
         (push 'car extracting-sequence)
         (setf scrawl-current-obj (car scrawl-current-obj))) 
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%")))
  (display-scrawl-result))
 
(defun scrawl-cdr-cmd ()
  (declare (special scrawl-current-obj extracting-sequence))
  (cond ((consp scrawl-current-obj)
         (push 'cdr extracting-sequence)
         (pop scrawl-current-obj))
;         (setf *scrawl-current-obj* (cdr *scrawl-current-obj*)))  ;; Symmetry above???
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%"))) 
  (display-scrawl-result))

(defun scrawl-back-up-cmd () 
  (declare (special scrawl-object scrawl-current-obj extracting-sequence))
  (cond ((null extracting-sequence) (format t "~&Already at beginning of object."))
        (t (pop extracting-sequence) 
           (setf scrawl-current-obj (extract-obj extracting-sequence scrawl-object))))
  (display-scrawl-result))

(defun scrawl-start-cmd ()
  (declare (special scrawl-object scrawl-current-obj extracting-sequence))
  (setf scrawl-current-obj scrawl-object
        extracting-sequence nil)
  (display-scrawl-result))

;;;
;;;    SEQ is the EXTRACTING-SEQUENCE, a list of CAR/CDR symbols indicating how the top-level object
;;;    has been traversed to this point.
;;;    
(defun extract-obj (seq obj) 
  (reduce #'funcall seq :initial-value obj :from-end t))

(defun get-car/cdr-string ()
  (declare (special scrawl-object extracting-sequence))
  (if (null extracting-sequence)
      (format nil "'~S" scrawl-object)
      (format nil "(c~Ar '~S)"
              (map 'string #'(lambda (x)
                               (ecase x
                                 (car #\a)
                                 (cdr #\d))) 
                    extracting-sequence)
              scrawl-object)))

(defun display-scrawl-result ()
  (declare (special display-width scrawl-current-obj))
  (let* ((*print-pretty* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-circle* t)
         (extract-string (get-car/cdr-string))
         (text (if (> (length extract-string) display-width) 
                   (ellide extract-string display-width)
                   extract-string)))
    (sdraw scrawl-current-obj :display-width display-width)
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

(defclass tty (renderer)
  ((textline-array :reader textline-array)
   (textline-lengths :reader textline-lengths)))

(defmethod initialize-instance :after ((tty tty) &rest initargs)
  (declare (ignore initargs))
  (with-slots (display-width num-lines textline-array textline-lengths) tty
    (setf textline-array (make-array num-lines)
          textline-lengths (make-array num-lines :initial-element 0))
    (dotimes (i (length textline-array))
      (setf (aref textline-array i) 
            (make-string display-width)))) )

(defmethod draw-structure ((tty tty) directions) 
  (follow-directions tty directions) 
  (dump-display tty))

(defun follow-directions (tty dirs &optional is-car) 
  (ecase (car dirs)
    (cons (draw-cons tty dirs))
    ((atom msg) (draw-msg tty dirs is-car))))

(defun draw-cons (tty directions)
  (destructuring-bind (type row col car-component cdr-component) directions
    (declare (ignore type))
    (with-slots (textline-array textline-lengths) tty
      (let ((line (aref textline-array row))
            (h-arrow-start (+ col *cons-cell-flatsize*))
            (h-arrowhead-col (1- (third cdr-component))))
        (char-blt tty row col *cons-string*) 
        (do ((i h-arrow-start (1+ i)))
            ((>= i h-arrowhead-col))
          (setf (aref line i) *cons-h-arrowshaft-char*))
        (setf (aref line h-arrowhead-col) *cons-h-arrowhead-char*)
        (setf (aref textline-lengths row) (1+ h-arrowhead-col)) 
        (char-blt tty (+ row 1) (+ col 1) *cons-v-line*)
        (char-blt tty (+ row 2) (+ col 1) *cons-v-arrowhead*)
        (follow-directions tty car-component t)
        (follow-directions tty cdr-component)))) )

(defun draw-msg (tty directions is-car)
  (destructuring-bind (type row col string) directions
    (declare (ignore type))
    (char-blt tty 
              row
              (+ col (if (and is-car (<= (length string) *cons-v-arrow-offset-threshold*)) 
                         *cons-v-arrow-offset-value*
                         0))
              string)))

(defun char-blt (tty row start-col string)
  "Clear out the existing text in ROW up to position START-COL."
  (with-slots (textline-array textline-lengths) tty
    (let ((spos (aref textline-lengths row))
          (line (aref textline-array row))) 
      (fill line #\Space :start spos :end start-col)
      (replace line string :start1 start-col)
      (setf (aref textline-lengths row)
            (+ start-col (length string)))) ))

(defun dump-display (tty)
  "Actually print the structure to the screen."
  (with-slots (textline-array textline-lengths) tty
    (terpri)
    (loop for i from 0 below (length textline-array)
          for len = (aref textline-lengths i)
          for line = (aref textline-array i)
          while (plusp len)
          do (format t "~&~A" (subseq line 0 len)))
    (terpri)))
