;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch11.lisp
;;;;
;;;;   Started:            Sun Nov 15 15:37:25 2020
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

(defpackage :ch11 (:use :common-lisp :test) (:shadow :member :assoc :length :nth :union :reverse :intersection))

(in-package :ch11)

;;;
;;;    11.1
;;;
(defun member (obj set)
  (dolist (elt set nil)
    (when (eql elt obj)
      (return t))))

(deftest test-member ()
  (check
   (member 'a '(a b c d))
   (member 'd '(a b c d))
   (not (member 'z '(a b c d)))
   (not (member 'z '()))) )

;;;
;;;    11.2
;;;    
(defun assoc (key table)
  (dolist (entry table nil)
    (when (eql (car entry) key)
      (return entry))))

(deftest test-assoc ()
  (check
   (assoc 'a '((a 1) (b 2) (c 3)))
   (assoc 'b '((a 1) (b 2) (c 3)))
   (not (assoc 'z '((a 1) (b 2) (c 3))))
   (not (assoc 'z '()))) )

;;;
;;;    11.3
;;;    
(defun check-all-odd (l)
  (if (endp l) 
      t
      (destructuring-bind (elt . more) l
        (format t "Checking ~D...~%" elt)
        (if (not (oddp elt))
            nil
            (check-all-odd more)))) )

;;;
;;;    11.4
;;;    
(defun length (l)
  (let ((length 0))
    (dolist (_ l length)
;      (declare (ignore _)) ; Can't ignore...Actually read in macro expansion!
      (incf length))))

;;;
;;;    Ha! Without (declare (ignore _))
;;;    CMUCL:
;   (LET (#)
;     (TAGBODY #))
; Note: Variable _ defined but never used.
;;;
;;;    With the declaration:
;;;    
;   (LET (#)
;     (DECLARE #)
;     _
;     LENGTH)
; Note: Reading an ignored variable: _.

(deftest test-length ()
  (check
   (= (length '()) 0)
   (= (length '(a)) 1)
   (= (length '(a b c)) 3)
   (= (length '((a 1) (b 2))) 2)))

;;;
;;;    11.5
;;;    
(defun nth (i l)
  (let ((n i))
    (dolist (elt l)
      (if (zerop n)
          (return elt)
          (decf n)))) )

(deftest test-nth ()
  (check
   (eql (nth 0 '(a b c)) 'a)
   (eql (nth 1 '(a b c)) 'b)
   (eql (nth 3 '(a b c)) '())))

;;;
;;;    11.6
;;;    
(defun union (a b)
  (let ((result b))
    (dolist (elt a result)
      (unless (member elt b)
        (push elt result)))) )

(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a)))

(deftest test-union ()
  (check
   (set-equal (union '(a b) '(a b)) '(a b))
   (set-equal (union '(a b c) '(d e f)) '(a b c d e f))
   (set-equal (union '(b c a) '(a d e)) '(a b c d e))
   (set-equal (union '() #1='(a b c d)) (cl:union '() #1#))
   (set-equal (union #1# '()) (cl:union #1# '()))
   (set-equal (union #1# #2='(1 2 3 4)) (cl:union #1# #2#))
   (set-equal (union #1# #1#) (cl:union #1# #1#))
   (set-equal (union #1# #3='(d c b a)) (cl:union #1# #3#))
   (set-equal (union #1# #4='(b d)) (cl:union #1# #4#))))

;;;
;;;    11.7
;;;    "Correct" IT-INTERSECTION (pg. 345) by returing elts in same order as input.
;;;
(defun intersection (a b)
  (let ((result '()))
    (dolist (elt a (nreverse result))
      (when (member elt b)
        (push elt result)))) )

(deftest test-intersection ()
  (check
   (set-equal (intersection '(a b) '(a b)) '(a b))
   (set-equal (intersection '(a b c) '(d e f)) '())
   (set-equal (intersection '(b c a) '(a d e)) '(a))
   (set-equal (intersection '() #1='(a b c d)) (cl:intersection '() #1#))
   (set-equal (intersection #1# '()) (cl:intersection #1# '()))
   (set-equal (intersection #1# #2='(1 2 3 4)) (cl:intersection #1# #2#))
   (set-equal (intersection #1# #1#) (cl:intersection #1# #1#))
   (set-equal (intersection #1# #3='(d c b a)) (cl:intersection #1# #3#))
   (set-equal (intersection #1# #4='(b d)) (cl:intersection #1# #4#))))

;;;
;;;    11.8
;;;    
(defun reverse (l)
  (let ((result '()))
    (dolist (elt l result)
      (push elt result))))

(deftest test-reverse ()
  (check
   (equal (reverse '()) (cl:reverse '()))
   (equal (reverse '(a)) (cl:reverse '(a)))
   (equal (reverse #1='(a b c d)) (cl:reverse #1#))))

;;;
;;;    11.9
;;;
(defun check-all-odd (l)
  (do ((list l (rest list)))
      ((endp list) t)
    (format t "Checking ~D...~%" (first list))
    (when (not (oddp (first list)))
      (return nil))))

;;;
;;;    11.10
;;;    
(defun launch (n)
  (dotimes (i n)
    (format t "~D..." (- n i)))
  (format t "Blast off!~%"))

(defun launch (n)
  (loop for i from n downto 1
        do (format t "~D..." i))
  (format t "Blast off!~%"))

;;;
;;;    11.11
;;;    
(defun find-largest (ns)
  (do ((l (rest ns) (rest l))
       (biggie (first ns) (max biggie (first l))))
      ((endp l) biggie)))

(deftest test-find-largest ()
  (check
   (= (find-largest '(1 2 3)) 3)
   (= (find-largest '(3 2 1)) 3)
   (= (find-largest '(1 1 1)) 1)))

;;;
;;;    11.12
;;;    
(defun power-of-2 (n)
  (do ((i n (1- i))
       (pow 1 (* pow 2)))
      ((zerop i) pow)))

(defun power-of-2-touretzky (n)
  (do ((i 0 (1+ i))
       (result 1 (+ result result))) ; !!
      ((= i n) result)))

(defun power (n)
  (ash 1 n))

(deftest test-power-of-2 ()
  (check
   (= (power-of-2 0) (power-of-2-touretzky 0) (power 0) 1)
   (= (power-of-2 1) (power-of-2-touretzky 1) (power 1) 2)
   (= (power-of-2 2) (power-of-2-touretzky 2) (power 2) 4)
   (= (power-of-2 8) (power-of-2-touretzky 8) (power 8) 256)))

;;;
;;;    11.13
;;;    
(defun first-non-integer (l)
  (dolist (elt l 'none)
    (unless (integerp elt)
      (return elt))))

(deftest test-first-non-integer ()
  (check
   (eq (first-non-integer '()) 'none)
   (eq (first-non-integer '(2 4 6 8)) 'none)
   (eq (first-non-integer '(:z :z :z)) :z)
   (eq (first-non-integer '(2 4 6 gugs 8)) 'gugs)))

;;;
;;;    11.18
;;;    
;; (do ((i 0 (1+ i)))
;;     ((= i 5) i)
;;   (format t "~&I = ~D" i))

;;;
;;;    11.21
;;;    FIBONACCI-A and FIBONACCI-B are equivalent. However, FIBONACCI-A is a
;;;    little confused. The semantics become clearer by renaming the 2 variables
;;;    as in FIBONACCI-A1/B1. Here, FIBONACCI-B1 is consistent. At each iteration
;;;    CURRENT holds the current Fibonacci number and NEXT holds the next in the sequence.
;;;    On the other hand, while FIBONACCI-A1 computes the correct value, the state of
;;;    NEXT is misleading. It progresses as 1, 0, 1, 1, 2, ...
;;;    
(defun fibonacci-a (n)
  (do ((i 0 (1+ i))
       (f0 0 (+ f0 f1))
       (f1 1 f0))
      ((= i n) f0)))

(defun fibonacci-b (n)
  (do ((i 0 (1+ i))
       (f0 0 f1)
       (f1 1 (+ f0 f1)))
      ((= i n) f0)))

(defun fibonacci-a1 (n)
  (do ((i 0 (1+ i))
       (current 0 (+ current next))
       (next 1 current))
      ((= i n) current)))

(defun fibonacci-b1 (n)
  (do ((i 0 (1+ i))
       (current 0 next)
       (next 1 (+ current next)))
      ((= i n) current)))

;; (loop for i from 0 to 20 collect (fibonacci i))
;; (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

(defun fibonacci-loop (n)
  (loop for i upto n
        for f0 = 0 then f1
        and f1 = 1 then (+ f0 f1)
        finally (return f0)))

;;;
;;;    Clearest!
;;;    
(defun fibonacci-tr-stu (n)
  (labels ((fibonacci-aux (current next i)
             (cond ((= i n) current)
                   (t (fibonacci-aux next (+ current next) (1+ i)))) ))
    (fibonacci-aux 0 1 0)))

(defun fibonacci-touretzky-do* (n)
  (do* ((i 0 (1+ i))
        (f0 0 f1)
        (f1 1 f2)
        (f2 1 (+ f0 f1)))
       ((= i n) f0)))

;;;
;;;    Semantically equivalent to LOOP version.
;;;    
(defun fibonacci-touretzky-do (n)
  (do ((i 0 (1+ i))
       (f0 0 f1)
       (f1 1 (+ f0 f1)))
      ((= i n) f0)))

(defun fibonacci-shift (n)
  (let ((current 0)
        (next 1))
    (dotimes (i n current)
      (shiftf current next (+ current next)))) )

;;;
;;;    TIME experiments
;;;    
(defun addup (n)
  (do ((i 0 (1+ i))
       (sum 0 (+ sum i)))
      ((> i n) sum)))

(defun addup-loop (n)
  (loop for i from 0 to n summing i))

(defun addup-tr (n)
  (labels ((addup (i sum)
             (cond ((> i n) sum)
                   (t (addup (1+ i) (+ sum i)))) ))
    (addup 0 0)))

(defun addup-pascal (n)
  (/ (* n (1+ n)) 2))

(deftest test-addup ()
  (check
   (= (addup 0) (addup-loop 0) (addup-tr 0) (addup-pascal 0))
   (= (addup 1) (addup-loop 1) (addup-tr 1) (addup-pascal 1))
   (= (addup 100) (addup-loop 100) (addup-tr 100) (addup-pascal 100))
   (= (addup 1000000) (addup-loop 1000000) (addup-tr 1000000) (addup-pascal 1000000))))

;;;
;;;    The DO version is actually slowest in SBCL. LOOP is fastest then tail recursion!
;;;    (Of course Pascal kills everybody else!)
;(dolist (f '(addup addup-loop addup-tr addup-pascal)) (time (funcall f 1000000)))
