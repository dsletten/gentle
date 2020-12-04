;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               discrimination-net-clos.lisp
;;;;
;;;;   Started:            Thu Nov 26 23:13:42 2020
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

(defpackage :discrimination-net-clos 
  (:use :common-lisp :test :lang)
  (:export :defnet 
           :make-node :node :name :question :yes-case :no-case 
           :make-discrimination-net :find-node :process-node :add-node :category :nodes
           :answer :text))

(in-package :discrimination-net-clos)

;;;
;;;    The YES-CASE and NO-CASE slots are references to other NODEs (or terminal strings!).
;;;    However, referencing other NODE objects directly creates a timing
;;;    problem. What if those instances don't yet exist while the current
;;;    NODE is being instantiated?
;;;    Instead these slots simply hold the names of (possibly future) NODEs.
;;;    
(defclass node ()
  ((name :reader name :initarg :name)
   (question :reader question :initarg :question)
   (yes-case :reader yes-case :initarg :yes-case :type (or symbol answer))
   (no-case :reader no-case :initarg :no-case :type (or symbol answer))))

(defun make-node (name question yes no)
  (make-instance 'node :name name :question question :yes-case (make-case yes) :no-case (make-case no)))

(defun make-case (node-case)
  (if (stringp node-case)
      (make-instance 'answer :text node-case)
      node-case))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "~A ~S" (name n) (question n))))

(defclass answer ()
  ((text :reader text :initarg :text)))

(defclass discrimination-net ()
  ((category :reader category :initarg :category)
   (nodes :reader nodes :initform (make-hash-table))))

(defun make-discrimination-net (category node-list)
  (let ((net (make-instance 'discrimination-net :category category)))
    (with-slots (nodes) net
      (dolist (node node-list)
        (setf (gethash (name node) nodes) node)))
    net))

(defmethod print-object ((net discrimination-net) stream)
  (print-unreadable-object (net stream :type t)
    (format stream "~S Node~P: ~:*~D" (category net) (hash-table-count (nodes net)))) )

(defmacro defnet (sym category &rest node-list)
  (let ((nodes (mapcar #'(lambda (node) 
                           (apply #'make-node node))
                       node-list)))
  `(defparameter ,sym (make-discrimination-net ',category ',nodes))))

(defgeneric find-node (net name)
  (:documentation "Locate a node specified by NAME in NET."))
(defmethod find-node ((net discrimination-net) (name symbol))
  (with-slots (nodes) net
    (gethash name nodes)))

(defgeneric process-node (net name)
  (:documentation "Process the node specified by NAME based on input from user."))
(defmethod process-node ((net discrimination-net) (name symbol))
  (let ((node (find-node net name)))
    (cond ((null node) (format t "Node ~A not yet defined.~%" name))
          (t (if (yes-or-no-p (format nil "~A " (question node)))
                 (yes-case node)
                 (no-case node)))) ))

(defgeneric add-node (net node)
  (:documentation "Add a node to the discrimination net."))
(defmethod add-node :around ((net discrimination-net) (node node))
  (with-slots (nodes) net
    (cond ((gethash (name node) nodes)
           (warn "A node named '~A' already exists.~%" (name node))
           (when (y-or-n-p "Do you wish to replace it? ")
               (call-next-method)))
          (t (call-next-method)))) )
(defmethod add-node ((net discrimination-net) (node node))
  (with-slots (nodes) net
    (setf (gethash (name node) nodes) node)))

(defgeneric run (net &optional start)
  (:documentation "Evaluate the discrimination net based on user input."))
(defmethod run ((net discrimination-net) &optional (start 'start))
  (labels ((execute (current-node)
             (typecase current-node
               (null (format t "At a loss here...~%"))
               (answer (format t "~A~%" (text current-node)))
               (otherwise (execute (process-node net current-node)))) ))
    (execute start)))

(defnet *car* "Engine diagnosis"
  (start "Does the engine turn over?" engine-turns-over engine-wont-turn-over)
  (engine-turns-over "Will the engine run for any period of time?" engine-will-run-briefly engine-wont-run)
  (engine-wont-run "Is there gas in the tank?" gas-in-tank "Fill the tank and try starting the engine again.")
  (engine-wont-turn-over "Do you hear any sound when you turn the key?" sound-when-turn-key no-sound-when-turn-key)
  (no-sound-when-turn-key "Is the battery voltage low?" "Replace the battery" battery-voltage-ok)
  (battery-voltage-ok "Are the battery cables dirty or loose?" "Clean the cables and tighten the connections." battery-cables-good)
  (engine-will-run-briefly "Does the engine stall when cold but not when warm?" check-idle-speed unknown-problem)
  (check-idle-speed "Is the cold idle speed at least 700 RPM?" unknown-problem "Adjust the idle speed."))

(defun read-node (net)
  (let ((name (read-from-string (prompt-read "Enter node name: " :allow-empty nil)))
        (question (prompt-read "Enter diagnostic question: " :allow-empty nil))
        (yes (read-from-string (prompt-read "What is the affirmative response? " :allow-empty nil))) ; String or symbol???
        (no (read-from-string (prompt-read "What is the negative response? " :allow-empty nil))))
    (add-node net (make-node name question yes no))))

