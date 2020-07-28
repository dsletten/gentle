;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               real-number.lisp
;;;
;;;   STARTED:            Sat Jan  7 22:57:33 HST 2006
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

;(load "fsm")

(defnode start)
(defnode int-digit)
(defnode decimal-point)
(defnode fraction-digit)
(defnode exponent)
(defnode exponent-sign)
(defnode exponent-digit)
(defnode end)

(defarc start int-digit 
(defarc start dime have-10 "Clink!")
(defarc start coin-return start "Nothing to return.")
(defarc have-5 nickel have-10 "Clunk!")
(defarc have-5 dime have-15 "Clink!")
(defarc have-5 coin-return start "Returned five cents.")
(defarc have-10 nickel have-15 "Clunk!")
(defarc have-10 dime have-20 "Clink!")
(defarc have-10 coin-return start "Returned ten cents.")
(defarc have-15 nickel have-20 "Clunk!")
(defarc have-15 dime have-20 "Nickel change.")
(defarc have-15 gum-button end "Deliver gum.")
(defarc have-15 coin-return start "Returned fifteen cents.")
(defarc have-20 nickel have-20 "Nickel returned.")
(defarc have-20 dime have-20 "Dime returned.")
(defarc have-20 gum-button end "Deliver gum, nickel change.")
(defarc have-20 mint-button end "Deliver mints.")
(defarc have-20 coin-return start "Returned twenty cents.")

    
