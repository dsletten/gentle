;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               fsm.lisp
;;;;
;;;;   Started:            Mon Dec 21 02:08:03 2020
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

(defpackage :fsm (:use :common-lisp :test))

(in-package :fsm)

;;;
;;;    Returns the exact coins already input when :COIN-RETURN is encountered.
;;;    (Not an "equivalent" value as Touretzky's does.)
;;;    
(defun vend (inputs)
  (labels ((start (inputs coins)
             (ecase (first inputs)
               (:coin-return coins)
               (:dime 
                (accept-dime)
                (have-10 (rest inputs) (cons (first inputs) coins)))
               (:nickel
                (accept-nickel)
                (have-5 (rest inputs) (cons (first inputs) coins)))) )
           (have-5 (inputs coins)
             (ecase (first inputs)
               (:coin-return coins)
               (:dime
                (accept-dime)
                (have-15 (rest inputs) (cons (first inputs) coins)))
               (:nickel
                (accept-nickel)
                (have-10 (rest inputs) (cons (first inputs) coins)))) )
           (have-10 (inputs coins)
             (ecase (first inputs)
               (:coin-return coins)
               (:dime
                (accept-dime)
                (have-20 (rest inputs) (cons (first inputs) coins)))
               (:nickel
                (accept-nickel)
                (have-15 (rest inputs) (cons (first inputs) coins)))) )
           (have-15 (inputs coins)
             (ecase (first inputs)
               (:coin-return coins)
               (:gum-button (list :gum))
               (:nickel
                (accept-nickel)
                (have-20 (rest inputs) (cons (first inputs) coins)))) )
           (have-20 (inputs coins)
             (ecase (first inputs)
               (:coin-return coins)
               (:gum-button (list :gum :nickel))
               (:mint-button (list :mint))))
           (accept-nickel () (format t "Clunk!~%"))
           (accept-dime () (format t "Clink!~%")))
    (start inputs '())))

(deftest test-vend ()
  (check
   (equal (vend '(:nickel :dime :gum-button)) '(:GUM))
   (equal (vend '(:nickel :nickel :nickel :gum-button)) '(:GUM))
   (equal (vend '(:nickel :nickel :nickel :nickel :gum-button)) '(:GUM :NICKEL))
   (equal (vend '(:nickel :nickel :nickel :nickel :mint-button)) '(:MINT))
   (equal (vend '(:nickel :dime :nickel :gum-button)) '(:GUM :NICKEL))
   (equal (vend '(:dime :nickel :nickel :gum-button)) '(:GUM :NICKEL))
   (equal (vend '(:nickel :nickel :dime :gum-button)) '(:GUM :NICKEL))
   (equal (vend '(:dime :dime :gum-button)) '(:GUM :NICKEL))
   (equal (vend '(:dime :dime :mint-button)) '(:MINT))
   (equal (vend '(:dime :nickel :nickel :mint-button)) '(:MINT))
   (equal (vend '(:nickel :nickel :dime :mint-button)) '(:MINT))
   (equal (vend '(:nickel :coin-return)) '(:nickel))
   (equal (vend '(:nickel :dime :coin-return)) '(:dime :nickel))
   (equal (vend '(:nickel :nickel :dime :coin-return)) '(:dime :nickel :nickel))))


(defstruct (node (:print-function print-node))
  name
  (inputs '())
  (outputs '()))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<Node ~A>" (node-name node)))

(defstruct (arc (:print-function print-arc))
  from
  to
  label
  action)

(defun print-arc (arc stream depth)
  (declare (ignore depth))
  (format stream "#<ARC ~A ---~A---> ~A>" (node-name (arc-from arc)) (arc-label arc) (node-name (arc-to arc))))

(defvar *nodes* '())
(defvar *arcs* '())
;(defvar *current-node* nil)

(defun initialize ()
  (setf *nodes* '()
        *arcs* '()))
;        *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

;;;
;;;    Preserve order of nodes.
;;;    
(defun add-node (name)
  (unless (find-node name :strict nil)
    (let ((node (make-node :name name)))
      (setf *nodes* (nconc *nodes* (list node)))
      node)))

;;;
;;;    Apparently ADD-ARC wants to detect non-existent NODEs => ERROR
;;;    But ADD-NODE needs to be able to handle new NODEs.
;;;    
(defun find-node (name &key (strict t))
  (or (find name *nodes* :key #'node-name)
      (if strict
          (error "No node named ~A exists." name)
          nil)))

;; (defun find-node (name)
;;   (find name *nodes* :key #'node-name))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (arc (make-arc :from from :label label :to to :action action)))
    (unless (find-arc arc)
      (setf *arcs* (nconc *arcs* (list arc))
            (node-outputs from) (nconc (node-outputs from) (list arc))
            (node-inputs to) (nconc (node-inputs to) (list arc)))
      arc)))

(defun find-arc (arc)
  (find arc *arcs* :test #'equalp))

;;;
;;;    Don't need bullshit special variables...
;;;    
;; (defun fsm (&optional (start-node 'start))
;;   (let ((current-node (find-node start-node)))
;;     (declare (special current-node))
;;     (do ()
;;         ((null (node-outputs current-node)))
;;       (one-transition))))

;; (defun one-transition ()
;;   (declare (special current-node))
;;   (format t "State ~A.  Input: " (node-name current-node))
;;   (let* ((*read-eval* nil)
;;          (ans (read))
;;          (arc (find ans (node-outputs current-node) :key #'arc-label)))
;;     (cond ((null arc)
;;            (format t "No arc from ~A has label ~A.~%" (node-name current-node) ans)
;;            nil)
;;           (t (format t "~A ~%" (arc-action arc))
;;              (setf current-node (arc-to arc)))) ))

;; (defun fsm (&optional (start-node 'start))
;;   (do ((current-node (find-node start-node) 
;;                      (transition current-node)))
;;       ((null (node-outputs current-node)))) )

(defun terminalp (node)
  (null (node-outputs node)))

(defun fsm (&optional (node-name 'start))
  (labels ((process (node)
             (unless (terminalp node)
               (process (transition node)))) )
    (process (find-node node-name))))

(defun transition (node)
  (format t "State ~A.  Input: " (node-name node))
  (let* ((*read-eval* nil)
         (ans (read))
         (arc (find ans (node-outputs node) :key #'arc-label)))
    (cond ((null arc)
           (format t "No arc from ~A has label ~A.~%" (node-name node) ans)
           node)
          (t (format t "~A ~%" (arc-action arc))
             (arc-to arc)))) )

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode have-25)
(defnode end)

(defarc start   nickel               have-5  "Clunk!")
(defarc start   dime                 have-10 "Clink!")
(defarc start   quarter              have-25 "Ker-chunk!")
(defarc start   coin-return          start   "Nothing to return.")

(defarc have-5  nickel               have-10 "Clunk!")
(defarc have-5  dime                 have-15 "Clink!")
(defarc have-5  quarter              have-25 "Nickel change.")
(defarc have-5  coin-return          start   "Returned five cents.")

(defarc have-10 nickel               have-15 "Clunk!")
(defarc have-10 dime                 have-20 "Clink!")
(defarc have-10 quarter              have-25 "Dime change.")
(defarc have-10 coin-return          start   "Returned ten cents.")

(defarc have-15 nickel               have-20 "Clunk!")
(defarc have-15 dime                 have-25 "Clink!")
(defarc have-15 quarter              have-25 "Fifteen cents change.")
(defarc have-15 gum-button           end     "Deliver gum.")
(defarc have-15 coin-return          start   "Returned fifteen cents.")

(defarc have-20 nickel               have-25 "Clunk!")
(defarc have-20 dime                 have-25 "Nickel change.")
(defarc have-20 quarter              have-25 "Twenty cents change.")
(defarc have-20 gum-button           end     "Deliver gum, nickel change.")
(defarc have-20 mint-button          end     "Deliver mints.")
(defarc have-20 coin-return          start   "Returned twenty cents.")

(defarc have-25 nickel               have-25 "Nickel returned.")
(defarc have-25 dime                 have-25 "Dime returned.")
(defarc have-25 quarter              have-25 "Quarter returned.")
(defarc have-25 gum-button           end     "Deliver gum, dime change.")
(defarc have-25 mint-button          end     "Deliver mints, nickel change.")
(defarc have-25 chocolate-bar-button end     "Deliver chocolate bar.")
(defarc have-25 coin-return          start   "Returned twenty-five cents.")
