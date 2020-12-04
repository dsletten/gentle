;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               discrimination-net-struct.lisp
;;;;
;;;;   Started:            Thu Nov 26 23:13:40 2020
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
;;;;   - See Slade 20 questions
;;;;   - AI Programming 2e
;;;;   - PAIP (trie)
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :discrimination-net-struct (:use :common-lisp :test :lang))

(in-package :discrimination-net-struct)

(defstruct node 
  (name)
  (question)
  (yes-case)
  (no-case))

(defvar *node-list* '())

(defun init ()
  (setf *node-list* '()))

(defun add-node (name question yes no)
  (pushnew (make-node :name name :question question :yes-case yes :no-case no) *node-list* :key #'node-name))

(defun find-node (name)
  (find name *node-list* :key #'node-name))

(defun process-node (name)
  (let ((node (find-node name)))
    (cond ((null node) (format t "Node ~A not yet defined.~%" name))
          (t (if (yes-or-no-p (format nil "~A " (node-question node))) ; !!
                 (node-yes-case node)
                 (node-no-case node)))) ))
;; (defun run ()
;;   (do ((current-node (process-node 'start) 
;;                      (process-node current-node)))
;;       ((not (node-p current-node))
;;        (etypecase current-node
;;          (nil (format t "At a loss here...~%"))
;;          (string (format t "~A~%" current-node)))) ))

(defun run ()
  (do ((current-node 'start))
      ((typecase current-node
         (null (format t "At a loss here...~%") t)
         (string (format t "~A~%" current-node) t)
         (otherwise nil)))
    (setf current-node (process-node current-node))))

(defun run ()
  (labels ((execute (current-node)
             (typecase current-node
               (null (format t "At a loss here...~%"))
               (string (format t "~A~%" current-node))
               (otherwise (execute (process-node current-node)))) ))
    (execute 'start)))

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
	  "Replace the battery"
	  'battery-voltage-ok)

(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)

(add-node 'engine-will-run-briefly
          "Does the engine stall when cold but not when warm?"
          'check-idle-speed
          'unknown-problem)

(add-node 'check-idle-speed
          "Is the cold idle speed at least 700 RPM?"
          'unknown-problem
          "Adjust the idle speed.")

(defun read-node ()
  (let ((name (read-from-string (prompt-read "Enter node name: " :allow-empty nil)))
        (question (prompt-read "Enter diagnostic question: " :allow-empty nil))
        (yes (read-from-string (prompt-read "What is the affirmative response? " :allow-empty nil))) ; String or symbol???
        (no (read-from-string (prompt-read "What is the negative response? " :allow-empty nil))))
    (add-node name question yes no)))
