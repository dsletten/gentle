;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               dna.lisp
;;;;
;;;;   Started:            Thu Nov 19 01:32:05 2020
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
(load "/home/slytobias/lisp/packages/collections.lisp")

(defpackage :dna (:use :common-lisp :test) (:shadow :t)) ; !!

(in-package :dna)

(defun complement-base (base)
  (ecase base
    (a 't)
    (t 'a)
    (g 'c)
    (c 'g)))

(deftest test-complement-base ()
  (check
   (eq (complement-base 'a) 't)
   (eq (complement-base 't) 'a)
   (eq (complement-base 'g) 'c)
   (eq (complement-base 'c) 'g)))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(deftest test-complement-strand ()
  (check
   (equal (complement-strand '(a g g t)) '(t c c a))))

(defun make-double (strand)
  (mapcar #'list strand (complement-strand strand)))

(defun make-double (strand)
  (mapcar #'(lambda (base) (list base (complement-base base))) strand))

(deftest test-make-double ()
  (check
   (equal (make-double '(g g a c t)) '((g c) (g c) (a t) (c g) (t a)))) )

;; (defun count-bases (strand)
;;   (let ((a 0)
;;         (c 0)
;;         (g 0)
;;         (t 0))
;;     (labels ((update-base (base)
;;                (ecase base
;;                  (a (incf a))
;;                  (t (incf t))
;;                  (g (incf g))
;;                  (c (incf c)))) )
;;       (do* ((l strand (rest l)) ; Assumes STRAND is not empty?!
;;             (elt (first l) (first l)))
;;            ((endp l) `((a ,a) (t ,t) (g ,g) (c ,c)))
;;         (if (listp elt)
;;             (mapc #'update-base elt)
;;             (update-base elt)))) ))

;; (defun count-bases (strand)
;;   (let ((a 0)
;;         (c 0)
;;         (g 0)
;;         (t 0))
;;     (labels ((update-base (base)
;;                (ecase base
;;                  (a (incf a))
;;                  (t (incf t))
;;                  (g (incf g))
;;                  (c (incf c)))) )
;;       (dolist (elt strand `((a ,a) (t ,t) (g ,g) (c ,c)))
;;         (if (listp elt)
;;             (mapc #'update-base elt)
;;             (update-base elt)))) ))

(defun count-bases (strand)
  (labels ((do-count-bases (strand counts)
             (dolist (elt strand counts)
               (if (listp elt)
                   (do-count-bases elt counts)
                   (incf (gethash elt counts 0)))) )
           (unpack (counts)
             (mapcar #'(lambda (base) (list base (gethash base counts))) '(a t g c))))
    (unpack (do-count-bases strand (make-hash-table)))) )

(defun set-equal (a b)
  (and (subsetp a b :test #'equal) (subsetp b a :test #'equal)))

(deftest test-count-bases ()
  (check
   (set-equal (count-bases '((g c) (a t) (t a) (t a) (c g))) '((A 3) (T 3) (G 2) (C 2)))
   (set-equal (count-bases '(a g t a c t c t)) '((A 2) (T 3) (G 1) (C 2)))) )

(defun prefixp (strand1 strand2)
  (cond ((endp strand1) cl:t)
        ((endp strand2) nil)
        ((equal (first strand1) (first strand2)) (prefixp (rest strand1) (rest strand2)))
        (cl:t nil)))

(defun prefixp (strand1 strand2)
  (let ((match (search strand1 strand2)))
    (if (null match)
        nil
        (zerop match))))

(deftest test-prefixp ()
  (check
   (prefixp '() '(g t c a t)) ; ??
   (prefixp '(g t c) '(g t c a t))
   (prefixp #1='(g t c a t) #1#)
   (not (prefixp '(g t c) '(a g g t c)))) )

(defun appearsp (strand1 strand2)
  (cond ((null strand2) nil)
        ((prefixp strand1 strand2) cl:t)
        (cl:t (prefixp strand1 (rest strand2)))) )

(defun appearsp (strand1 strand2)
  (search strand1 strand2))

(deftest test-appearsp ()
  (check
   (appearsp '(c a t) '(t c a t g))
   (not (appearsp '(c a t) '(t c c g t a)))) )

(defun coverp (strand1 strand2)
  "Does STRAND1 (possibly repeated) match STRAND2 entirely?"
  (labels ((check-cover (strand strand2)
             (cond ((endp strand2) (endp strand))
                   ((endp strand) (check-cover strand1 strand2))
                   ((equal (first strand) (first strand2)) (check-cover (rest strand) (rest strand2)))
                   (cl:t nil))))
    (check-cover strand1 strand2)))

(deftest test-coverp ()
  (check
   (coverp '(a g c) '(a g c))
   (coverp '(a g c) '(a g c a g c a g c))
   (not (coverp '(a g c) '(a g c a g c a g)))
   (not (coverp '(a g c) '(a g c t t g)))) )

(defun prefix (n strand)
  (cond ((zerop n) '())
        ((endp strand) '())
        (cl:t (cons (first strand) (prefix (1- n) (rest strand)))) ))

(defun prefix (n strand)
  (loop repeat n
        for elt in strand collect elt))

(deftest test-prefix ()
  (check
   (equal (prefix 1 '(c g a t t a g)) '(c))
   (equal (prefix 2 '(c g a t t a g)) '(c g))
   (equal (prefix 3 '(c g a t t a g)) '(c g a))
   (equal (prefix 4 '(c g a t t a g)) '(c g a t))
   (equal (prefix 8 #1='(c g a t t a g)) #1#)))

(defun kernel-1 (strand)
  "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
  (let ((length (length strand)))
    (dotimes (i length)
      (let ((prefix (prefix (1+ i) strand)))
        (when (coverp prefix strand)
          (return prefix)))) ))

;;;
;;;    D'oh! I got this right in 2002!
;;;    
;; (defun kernel (strand)
;;   "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
;;   (do* ((i 1 (1+ i))
;;         (prefix (prefix i strand) (prefix i strand)))
;;        (nil)
;;     (when (coverp prefix strand)
;;       (return prefix))))

(defun kernel-2 (strand)
  "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
  (do* ((i 1 (1+ i))
        (prefix (prefix i strand) (prefix i strand)))
       ((coverp prefix strand) prefix)))

(defun kernel-3 (strand)
  "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
  (loop for i from 1
        for prefix = (prefix i strand)
        when (coverp prefix strand)
        do (return prefix)))

(defun kernel-4 (strand)
  "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
  (labels ((find-kernel (i)
             (let ((prefix (prefix i strand)))
               (if (coverp prefix strand)
                   prefix
                   (find-kernel (1+ i)))) ))
    (find-kernel 1)))

(defun kernel (strand)
  "Find the shortest subsequence of STRAND that covers the STRAND. In the degenerate case, STRAND is its own kernel."
  (let ((q (collections:make-linked-queue)))
    (collections:enqueue q (first strand))
    (do ((l (rest strand) (rest l)))
        ((coverp (collections:elements q) strand) (collections:elements q))
      (collections:enqueue q (first l)))) )

;;;
;;;    The queue (KERNEL) is fastest.
;;;    
;(dolist (f '(kernel kernel-1 kernel-2 kernel-3 kernel-4)) (time (dotimes (i 10000) (funcall f '(a b c d e f g a b c d e f g a b c d e f g a b c d e f g a b c d e f g)))))

(deftest test-kernel ()
  (check
   (equal (kernel '(a g c a g c a g c)) '(a g c))
   (equal (kernel '(a a a a a)) '(a))
   (equal (kernel #1='(a g g t c)) #1#)))

(defconstant border "-----")
(defconstant link "  !  ")
(defconstant bond "  .  ")
;; (defun draw-dna (strand)
;;   (let ((length (length strand)))
;;     (draw-line length border)
;;     (draw-line length link)
;;     (draw-strand strand)
;;     (draw-line length bond)
;;     (draw-line length bond)
;;     (draw-strand (complement-strand strand))
;;     (draw-line length link)
;;     (draw-line length border)))

(defmacro draw-nested ((n s) &body body)
  `(progn (draw-line ,n ,s)
          ,@body
          (draw-line ,n ,s)))

(defun draw-dna (strand)
  (let ((length (length strand)))
    (draw-nested (length border)
      (draw-nested (length link)
        (draw-strand strand)
        (draw-nested (length bond))
        (draw-strand (complement-strand strand)))) ))

(defun draw-strand (strand)
  (dolist (base strand)
    (format cl:t "  ~A  " base))
  (terpri))

(defun draw-line (n s)
  (dotimes (i n)
    (format cl:t s))
  (terpri))
    
