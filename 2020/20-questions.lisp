;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               20-questions.lisp
;;;;
;;;;   Started:            Tue Dec  1 18:57:36 2020
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
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Touretzky/2020/discrimination-net-clos.lisp")

(defpackage :20-questions (:use :common-lisp :test :discrimination-net-clos :lang))

(in-package :20-questions)

(defclass game ()
  ((net :reader net :initarg :net)
   (cache :reader cache :initform (make-hash-table :test #'equalp))
   (category :reader category :initarg :category)))

(defmethod initialize-instance :after ((g game) &rest initargs)
  (declare (ignore initargs))
  (with-slots (net) g
    (loop for k being the hash-keys in (nodes net)
          for node = (gethash k (nodes net))
          for yes = (yes-case node)
          for no = (no-case node)
          when (typep yes 'answer)
          do (add-to-cache g (text yes))
          when (typep no 'answer)
          do (add-to-cache g (text no)))) )

(defun make-game (category net)
  (make-instance 'game :category category :net net))

(defun add-to-cache (game key)
  (with-slots (cache) game
    (setf (gethash key cache) t)))

(defmacro defgame (sym category &rest node-list)
  (let* ((nodes (mapcar #'(lambda (node) 
                            (destructuring-bind (name question yes no) node
;                            (apply #'make-node node)) ; Must make GAME-ANSWER!!
                              (make-node name question (make-game-case yes) (make-game-case no))))
                        node-list))
         (net (make-discrimination-net category nodes)))
    `(defparameter ,sym (make-game ,category ,net))))

(defun make-game-case (node-case)
  (etypecase node-case
    (symbol node-case)
    (list (make-instance 'game-answer :determiner (first node-case) :text (second node-case)))) )

(defun answer-exists-p (game text)
  (with-slots (cache) game
    (gethash text cache)))

(defclass game-answer (answer)
  ((determiner :reader determiner :initarg :determiner :initform "a"))) ; Fix...

(defun play (game &optional (start 'start))
  (with-slots (net) game
    (labels ((execute (current-node previous-node)
               (typecase current-node
                 (null (format t "At a loss here...~%"))
                 (answer (unless (check-answer current-node)
                           (add-game-node game current-node previous-node)))
                 (otherwise (execute (process-node net current-node) current-node)))) )
      (execute start nil))))

(defun check-answer (answer)
  (yes-or-no-p "Is it ~A ~A? " (determiner answer) (text answer)))

(defun add-game-node (game answer parent-node)
  (with-slots (nodes) (net game)
    (with-slots (yes-case no-case) (find-node (net game) parent-node)
      (let* ((new-answer (read-unique-answer "What is it then? " game))
             (new-question (prompt-read (format nil "What is a question that distinguishes between ~A ~A and ~A ~A? " (determiner new-answer) (text new-answer) (determiner answer) (text answer))
                                        :allow-empty nil))
             (response (prompt-read (format nil "How would you answer that for ~A ~A? " (determiner new-answer) (text new-answer)) :allow-empty nil))
             (new-node-name (gensym)))
        (if (eq answer yes-case)
            (setf yes-case new-node-name)
            (setf no-case new-node-name))
        (cond ((string-equal response "yes") (setf (gethash new-node-name nodes) (make-node new-node-name new-question new-answer answer)))
              ((string-equal response "no") (setf (gethash new-node-name nodes) (make-node new-node-name new-question answer new-answer))))
        (add-to-cache game (text new-answer)))) ))

(defun read-unique-answer (prompt game)
  (let ((response (prompt-read prompt :allow-empty nil)))
    (multiple-value-bind (determiner text) (find-determiner response)
      (cond ((answer-exists-p game text)
             (format t "Nice try. That item already exists.~%")
             (read-unique-answer prompt game))
            (t (make-instance 'game-answer :text text :determiner determiner)))) ))

(defun find-determiner (answer)
  (cond ((starts-with answer "a" :test #'char-equal) (values "a" (string-trim '(#\space) (subseq answer 1))))
        ((starts-with answer "an" :test #'char-equal) (values "an" (string-trim '(#\space) (subseq answer 2))))
        (t (values "a" answer)))) ; Fix...
        
(defgame *animals* "20 Questions Animals"
  (start "Does it purr?" purrs does-not-purr)
  (purrs "Does it run on regular gas?" ("a" "Ferrari") ("a" "cat"))
  (does-not-purr "Is it gray?" ("an" "elephant") not-gray)
  (not-gray "Is it bigger than a person?" bigger-than-person ("a" "monkey"))
  (bigger-than-person "Does it have stripes?" ("a" "tiger") no-stripes)
  (no-stripes "Is it a domestic animal?" ("a" "horse") ("a" "gorilla")))

;; (defnet *animals* "20 Questions Animals"
;;   (start "Does it purr?" "cat" "elephant"))

;; (defgame *animals* "Animals"
;;   (start "Does it purr?" ("a" "cat") ("an" "elephant")))
