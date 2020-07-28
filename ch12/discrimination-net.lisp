;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               discrimination-net.lisp
;;;
;;;   STARTED:            Wed Jul 24 16:51:10 2002
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
(defvar *node-list* ())

(defstruct node
  (name nil)
  (question "" :type string)
  (yes-case nil :type (or symbol null string))
  (no-case nil :type (or symbol null string)))

(defun add-node (name question yes no)
  (push (make-node :name name
		   :question question
		   :yes-case yes
		   :no-case no) *node-list*)
  name)

(defun find-node (name)
  (find-if #'(lambda (node) (eq (node-name node) name)) *node-list*))

(defun process-node (name)
  (let ((node (find-node name)))
    (cond (node
	   (if (y-or-n-p (node-question node))
	       (if (stringp (node-yes-case node))
		   (format t "~A~%" (node-yes-case node))
		   (process-node (node-yes-case node)))
	       (if (stringp (node-no-case node))
		   (format t "~A~%" (node-no-case node))
		   (process-node (node-no-case node)))) )
	  (t (format t "Node ~S not yet defined.~%" name)))) )

(defun run ()
  (process-node 'start))

(defun user-add-node ()
  (let* ((name (get-node-name))
	 (question (get-node-question name))
	 (yes (get-node-response "yes"))
	 (no (get-node-response "no")))
    (add-node name question yes no)))

(defun get-node-name ()
  (format t "Enter node name: ")
  (read))

(defun get-node-question (name)
  (format t "Enter a yes/no question appropriate to the ~S node: " name)
  (let ((s (read-line)))
    (if (string= s "")
	(get-node-question name)
	s)))

(defun get-node-response (case)
  (cond ((y-or-n-p "If the answer to the question is ~A is there another decision to be made? " case)
	 (format t "What is the next node name? ")
	 (read))
	(t (format t "What is the diagnosis? ")
	   (read-line))))

(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)

(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)

(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery."
	  'battery-voltage-ok)

(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)

(add-node 'engine-will-run-briefly
	  "Does the engine stall when cold but not when warm?"
	  'stalls-when-cold
	  'always-stalls)

(add-node 'stalls-when-cold
	  "Is the cold idle speed at least 700 rpm?"
	  'cold-idle-speed-ok
	  "Adjust the idle speed.")

	


			       