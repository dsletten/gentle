;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               fsm-compiled2.lisp
;;;;
;;;;   Started:            Sun Dec 27 03:58:50 2020
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

(defpackage :fsm-compiled2 (:use :common-lisp :test))

(in-package :fsm-compiled2)

(defun compile-arc (arc)
  (destructuring-bind (label to action) arc
    `((eql input ',label)
      (format t "~A~%" ,action)
      (,to (rest inputs)))) )

;; (defun compile-node (node)
;;   (destructuring-bind (name . arcs) node
;;     `(,name (inputs)
;;        (let ((input (first inputs)))
;;          (cond ((null inputs) ',name)
;;                ,@(mapcar #'compile-arc arcs)
;;                (t (error "No arc from ~A with label ~A." ',name input)))) )))

(defun compile-node (node)
  (destructuring-bind (name . arcs) node
    `(,name (inputs)
       (let ((input (first inputs)))
         (cond ((null inputs) ',name)
               ,@(mapcar #'compile-arc arcs)
               (t (missing-arc ',name input)))) )))

;; (defmacro compile-machine ()
;;   `(progn ,@(mapcar #'compile-node *nodes*)))

;; (defmacro defmachine (machine &rest nodes)
;;   `(defun ,machine (inputs)
;;      (labels ,(mapcar #'compile-node nodes)
;;        (start inputs))))

(defmacro defmachine (machine &rest nodes)
  `(defun ,machine (inputs)
     (labels (,@(mapcar #'compile-node nodes)
              (missing-arc (node-name label)
                (error "No arc from ~A with label ~A." node-name label)))
       (start inputs))))

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

;; (defmachine vend (start (nickel have-5 "Clunk!")
;;                         (dime have-10 "Clink!")
;;                         (quarter have-25 "Ker-chunk!")
;;                         (coin-return start "Nothing to return."))
;;                  (have-5 (nickel have-10 "Clunk!")
;;                          (dime have-15 "Clink!")
;;                          (quarter have-25 "Nickel change.")
;;                          (coin-return start "Returned five cents."))
;;                  (have-10 (nickel have-15 "Clunk!")
;;                           (dime have-20 "Clink!")
;;                           (quarter have-25 "Dime change.")
;;                           (coin-return start "Returned ten cents."))
;;                  (have-15 (nickel have-20 "Clunk!")
;;                           (dime have-25 "Clink!")
;;                           (quarter have-25 "Fifteen cents change.")
;;                           (gum-button end "Deliver gum.")
;;                           (coin-return start "Returned fifteen cents."))
;;                  (have-20 (nickel have-25 "Clunk!")
;;                           (dime have-25 "Nickel change.")
;;                           (quarter have-25 "Twenty cents change.")
;;                           (gum-button end "Deliver gum, nickel change.")
;;                           (mint-button end "Deliver mints.")
;;                           (coin-return start "Returned twenty cents."))
;;                  (have-25 (nickel have-25 "Nickel returned.")
;;                           (dime have-25 "Dime returned.")
;;                           (quarter have-25 "Quarter returned.")
;;                           (gum-button end "Deliver gum, dime change.")
;;                           (mint-button end "Deliver mints, nickel change.")
;;                           (chocolate-bar-button end "Deliver chocolate bar.")
;;                           (coin-return start "Returned twenty-five cents."))
;;                  (end))

(DEFUN VEND (INPUTS)
  (LABELS ((START (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'START)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Clunk!")
                      (HAVE-5 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Clink!")
                      (HAVE-10 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Ker-chunk!")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Nothing to return.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'START INPUT)))))
           (HAVE-5 (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'HAVE-5)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Clunk!")
                      (HAVE-10 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Clink!")
                      (HAVE-15 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Nickel change.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Returned five cents.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'HAVE-5 INPUT)))))
           (HAVE-10 (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'HAVE-10)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Clunk!")
                      (HAVE-15 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Clink!")
                      (HAVE-20 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Dime change.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Returned ten cents.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'HAVE-10 INPUT)))))
           (HAVE-15 (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'HAVE-15)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Clunk!")
                      (HAVE-20 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Clink!")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Fifteen cents change.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'GUM-BUTTON)
                      (FORMAT T "~A~%" "Deliver gum.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Returned fifteen cents.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'HAVE-15 INPUT)))))
           (HAVE-20 (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'HAVE-20)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Clunk!")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Nickel change.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Twenty cents change.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'GUM-BUTTON)
                      (FORMAT T "~A~%" "Deliver gum, nickel change.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'MINT-BUTTON)
                      (FORMAT T "~A~%" "Deliver mints.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Returned twenty cents.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'HAVE-20 INPUT)))))
           (HAVE-25 (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'HAVE-25)
                     ((EQL INPUT 'NICKEL)
                      (FORMAT T "~A~%" "Nickel returned.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'DIME)
                      (FORMAT T "~A~%" "Dime returned.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'QUARTER)
                      (FORMAT T "~A~%" "Quarter returned.")
                      (HAVE-25 (REST INPUTS)))
                     ((EQL INPUT 'GUM-BUTTON)
                      (FORMAT T "~A~%" "Deliver gum, dime change.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'MINT-BUTTON)
                      (FORMAT T
                              "~A~%"
                              "Deliver mints, nickel change.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'CHOCOLATE-BAR-BUTTON)
                      (FORMAT T "~A~%" "Deliver chocolate bar.")
                      (END (REST INPUTS)))
                     ((EQL INPUT 'COIN-RETURN)
                      (FORMAT T "~A~%" "Returned twenty-five cents.")
                      (START (REST INPUTS)))
                     (T (MISSING-ARC 'HAVE-25 INPUT)))))
           (END (INPUTS)
             (LET ((INPUT (FIRST INPUTS)))
               (COND ((NULL INPUTS) 'END)
                     (T (MISSING-ARC 'END INPUT)))))
           (MISSING-ARC (NODE-NAME LABEL)
             (ERROR "No arc from ~A with label ~A."
                    NODE-NAME
                    LABEL)))
    (START INPUTS)))

;;;
;;;    Fix END???
;;;    
(vend '(dime dime dime chocolate-bar-button dime))
Clink!
Clink!
Nickel change.
Deliver chocolate bar.
> Error: No arc from END with label DIME.
