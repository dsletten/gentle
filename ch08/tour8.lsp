;#!/usr/local/bin/clisp

;;
;   NAME:               tour8.lsp
;
;   STARTED:            010429
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;

;;;
;;;    8.2
;;;
(defun anyoddp (num-list)
  (if (null num-list)
      nil
      (if (oddp (first num-list))
	  t
	  (anyoddp (rest num-list)))) )

;;;
;;;    8.5
;;;
(defun add-up (num-list)
  (cond ((null num-list) 0)
	(t (+ (first num-list) (add-up (rest num-list)))) ) )

;;;
;;;    8.6
;;;
(defun alloddp (num-list)
  (cond ((null num-list) t)
	((evenp (first num-list)) nil)
	(t (alloddp (rest num-list)))) )

(defun all-oddp (l)
  (cond ((null l) t)
	((and (integerp (first l))        ;Check whether element is even an
					  ;integer first.
	      (evenp (first l))) nil)
	(t (all-oddp (rest l)))) )

;;;
;;;    Nice try...but doesn't work...
;;;
;;; (all-oddp '(2.0 4.0 6.0)) => T

;;;
;;;    Fixed (201103!)
;;;    
(defun all-oddp (l)
  (cond ((null l) t)
	((not (integerp (first l))) nil)
        ((evenp (first l)) nil)
	(t (all-oddp (rest l)))) )

;;;
;;;    8.7
;;;
(defun my-member (elt l)
  (cond ((null l) nil)
	((eql elt (first l)) l)
	(t (my-member elt (rest l)))) )

;;;
;;;    8.8
;;;
(defun my-assoc (key a-list)
  (cond ((null a-list) nil)
	((eql key (first (first a-list)))
	 (first a-list))
	(t (my-assoc key (rest a-list)))) )

;;;
;;;    8.9 / 8.28
;;;
(defun my-nth (n l)
  (cond ((null l) nil) ;This test is not strictly needed since (cdr ()) == ()
	((zerop n) (first l))
	(t (my-nth (1- n) (rest l)))) )

;;;
;;;    Touretzky:
;;;
(defun my-nth (n l)
  (cond ((zerop n) (first l)) ;Even if n > (length l),
			      ;(first (rest l)) will give the correct answer.
	(t (my-nth (1- n) (rest l)))) )

;;;
;;;    8.10
;;;    (x >= 0, y >= 0)
(defun plus (x y)
  (cond ((zerop y) x)
	(t (plus (add1 x) (sub1 y)))) )

(defun add1 (x)
  (+ x 1) )

(defun sub1 (x)
  (- x 1) )

;;;
;;;    8.14
;;;
(defun pung ()
  (pung) )

;;;
;;;    8.17
;;;
(defun find-first-odd (num-list)
  (cond ((null num-list) nil)
	((oddp (first num-list)) (first num-list))
	(t (find-first-odd (rest num-list)))) )

(defun find-first-odd (num-list)
  (find-if #'oddp num-list))

;;;
;;;    8.18
;;;
(defun last-element (l)
  (cond ((atom (cdr l))       ;End of the line.
	 (if (cdr l)          ;Is this not a proper list?
	     (cdr l)
	     (car l)))
	(t (last-element (cdr l)))) )

(defun last-element (l)
  (cond ((null (cdr l)) (car l))
	((atom (cdr l)) (cdr l))
	(t (last-element (cdr l)))) )

(defun last-element (l)
  (typecase (cdr l)
    (null (car l))
    (atom (cdr l))
    (t (last-element (cdr l)))) )

;; Touretzky's version (Only works for proper lists.)
(defun last-element (l)
  (cond ((atom (cdr l)) (car l))
	(t (last-element (cdr l)))) )

;;;
;;;    8.21
;;;
(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (1- n)))) ) )

(defun add-nums-1 (n)
  (/ (* n (1+ n)) 2) )

;;;
;;;    8.22
;;;    (Base case must terminate if 0 or 1 elements in list. Can't compare
;;;    (first l) and (second l) if only 1 element!)
;;;    
(defun all-equal (l)
  (cond ((null (rest l)) t)
	((not (equal (first l)
		     (second l)))
	 nil)
	(t (all-equal (rest l)))) )

(defun all-equal (l)
  (cond ((null l) t)
        ((null (rest l)) t)
        (t (destructuring-bind (a b . rest) l
               (and (equal a b)
                    (all-equal (rest l)))) )))

(defun all-equal (l)
  (destructuring-bind (&optional (a nil a-supplied-p) (b nil b-supplied-p) &rest more) l
    (or (endp l)
        (and a-supplied-p
             (not b-supplied-p))
        (and (equal a b)
             (all-equal (rest l)))) ))

(defun all-equal (l)
  (if (endp l)
      t
      (destructuring-bind (a . rest1) l
          (if (endp rest1)
              t
              (destructuring-bind (b . rest2) rest1
                (and (equal a b)
                     (all-equal rest1)))) )))

;;;
;;;    8.24
;;;
(defun count-down (n)
  (cond ((zerop n) ())
	(t (cons n (count-down (1- n)))) ) )

;;;
;;;    8.25
;;;
(defun fact-app (n)
  (apply #'* (count-down n)) )

;;;
;;;    8.26
;;;
(defun count-down-1 (n)
  (cond ((zerop n) (list 0))
	(t (cons n (count-down (1- n)))) ) )

(defun count-down-2 (n)
  (cond ((< n 0) ())
	(t (cons n (count-down (1- n)))) ) )

;;;
;;;    8.27
;;;
(defun square-list (num-list)
  (cond ((null num-list) ())
	(t (cons (* (first num-list) (first num-list))
		 (square-list (rest num-list)))) ) )

(defun square-list-1 (num-list)
  (mapcar #'(lambda (x) (* x x))
	  num-list) )

;;;
;;;    8.28
;;;
(defun my-nth (n l)
  (cond ((null l) nil)
	((zerop n) (car l))
	(t (my-nth (1- n) (cdr l)))) )

;;;
;;;    8.31
;;;
(defun compare-lengths (l1 l2)
  (cond ((and (null l1) (null l2)) 'same-length)
	((null l1) 'second-is-longer)
	((null l2) 'first-is-longer)
	(t (compare-lengths (rest l1) (rest l2)))) )

(defun compare-lengths (l1 l2)
  (cond ((and l1 l2) (compare-lengths (cdr l1) (cdr l2)))
	(l1 'first-is-longer)
	(l2 'second-is-longer)
	(t 'same-length)))

;;;
;;;    8.32 (Compare 8.41)
;;;
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
	((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
	(t (sum-numeric-elements (rest l)))) )

(defun sum-numeric-elements (l)
  (unless l (return-from sum-numeric-elements 0))
  (typecase (car l)
    (number (+ (car l) (sum-numeric-elements (cdr l))))
    (t (sum-numeric-elements (cdr l)))) )

;;;
;;;    8.33
;;;
(defun my-remove (elt l)
  (cond ((null l) ())
        ((eql elt (first l)) (my-remove elt (rest l)))
        (t (cons (first l) (my-remove elt (rest l)))) ) )

;;;
;;;    This works like built-in REMOVE-DUPLICATES.
;;;    
(defun my-remove-duplicates (l)
  (labels ((remove-duplicates-aux (l result)
	     (cond ((null l) (nreverse result))
               ((member (car l) (cdr l))
                (remove-duplicates-aux (cdr l) result))
               (t
                (remove-duplicates-aux (cdr l) (cons (car l) result)))) ))
    (remove-duplicates-aux l ())))

(defun my-remove-duplicates-2 (l)
  (labels ((remove-duplicates-aux (l result)
             (cond ((null l) result)
                   ((member (car l) result)
                    (remove-duplicates-aux (cdr l) result))
                   (t
                    (remove-duplicates-aux (cdr l) (cons (car l) result)))) ))
    (remove-duplicates-aux l ())))

;;;
;;;    8.34
;;;
(defun my-intersection (a b)
  (cond ((null a) ())
	((member (first a) b) (cons (first a) (my-intersection (rest a) b)))
	(t (my-intersection (rest a) b))) )

(defun my-union (a b)
  (cond ((null a) b)
	((member (first a) b) (my-union (rest a) b))
	(t (cons (first a) (my-union (rest a) b)))) )
;Changing the final line to:
;	(t (my-union (rest a) (cons (first a) b)))) )
;makes the function tail-recursive, but the symmetry with
;the two other set operations is lost.

;;;
;;;    8.35
;;;
(defun my-set-difference (a b)
  (cond ((null a) ())
	((member (first a) b) (my-set-difference (rest a) b))
	(t (cons (first a) (my-set-difference (rest a) b)))) )

;;;
;;;    8.36
;;;
(defun count-odd (num-list)
  (cond ((null num-list) 0)
	((oddp (first num-list)) (+ 1 (count-odd (rest num-list))))
	(t (count-odd (rest num-list)))) )

(defun count-odd-1 (num-list)
  (cond ((null num-list) 0)
	(t (+ (if (oddp (first num-list)) 1 0)
	      (count-odd-1 (rest num-list)))) ))

;;;
;;;    8.39
;;;
(defun count-atoms-bs (tree)
  (cond ((atom tree) 1)
	(t (+ (count-atoms-bs (first tree))
	      (count-atoms-bs (rest tree)))) ) )

(defun count-atoms (tree)
  (cond ((null tree) 0)
	((atom tree) 1)
	(t (+ (count-atoms (first tree))
	      (count-atoms (rest tree)))) ) )

;;;
;;;    8.40
;;;
(defun count-cons (tree)
  (cond ((atom tree) 0)
	(t (+ 1 
	      (count-cons (first tree))
	      (count-cons (rest tree)))) ) )

;;;
;;;    8.41 (Compare 8.32)
;;;
(defun sum-tree (tree)
  (cond ((numberp tree) tree)
	((atom tree) 0)
	(t (+ (sum-tree (first tree))
	      (sum-tree (rest tree)))) ) )
	
;;;
;;;    8.42
;;;
(defun my-subst (new old tree)
  (cond ((equal old tree) new)
	((atom tree) tree)
	(t (cons (my-subst new old (first tree))
		 (my-subst new old (rest tree)))) ) )

;;
;;    My original (only works if new/old are atoms)
;;    (Neither one works with lists if eql is used instead of equal)
(defun my-subst-1 (new old tree)
  (cond ((null tree) ())
	((atom tree) (if (equal old tree)
			 new
		         tree))
	(t (cons (my-subst-1 new old (first tree))
		 (my-subst-1 new old (rest tree)))) ) )

;;;
;;;    8.43
;;;    "flatten" => "append" (Not necessarily!)
;oops!
; (defun flatten (tree)
;   (cond ((atom tree) tree)
; 	(t (append (list (flatten (first tree)))
; 		   (flatten (rest tree)))) ) )

(defun flatten (tree)
  (cond ((null tree) ())
	((atom tree) (list tree))
	(t (append (flatten (first tree))
		   (flatten (rest tree)))) ) )

;;;
;;;    8.44
;;;    (Tree depth is max distance from top node to furthest terminal node.
;;;    For a list of atoms this is the same as its length.)
;;;
(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (max (tree-depth (first tree))
                     (tree-depth (rest tree)))) )) )

;;;
;;;    8.45
;;;    (By adding the first clause below we can distinguish between NIL as an
;;;    element and NIL as end of list. In other words, NIL as CAR vs. NIL as
;;;    CDR. Otherwise, there is no difference without the first clause.
;;;    With: (paren-depth '(a b () c)) => 2
;;;    Without: (paren-depth '(a b () c)) => 1
;;;    Either: (paren-depth '(a b (c))) => 2)
;;;
(defun paren-depth (tree)
  (cond ;((null tree) 1)
	((atom tree) 0)
	(t (max (+ (paren-depth (first tree)) 1)
		(paren-depth (rest tree)))) ) )

;;;
;;;    Original COUNT-UP (All tail-recursive)
;;;
(defun count-up (n)
  (count-up-aux n ()))

(defun count-up-aux (n result)
  (cond ((zerop n) result)
	(t (count-up-aux (1- n) (cons n result)))) )

(defun count-up-1 (n &optional result)
  (cond ((zerop n) result)
	(t (count-up-1 (1- n) (cons n result)))) )

(defun count-up-2 (n)
  (labels ((count-up-aux (n result)
	     (cond ((zerop n) result)
		   (t (count-up-aux (1- n) (cons n result)))) ))
    (check-type n (integer 0 *))
    (count-up-aux n ())))

;;;
;;;    Touretzky:
;;;
(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively (cnt n)
  (cond ((> cnt n) ())
	(t (cons cnt (count-up-recursively (+ cnt 1) n)))) )
	      
;;;
;;;    8.46
;;;
(defun count-up (n)
  (cond ((zerop n) nil)
	(t (append (count-up (1- n))
		   (list n)))) )

;;;
;;;    8.47
;;;
(defun make-loaf (n)
  (if (zerop n)
      ()
      (cons 'x (make-loaf (1- n)))) )

;;;
;;;    8.48
;;;
(defun bury (obj n)
  (cond ((zerop n) obj)
	(t (list (bury obj (1- n)))) ) )
;;
;;    Tail recursive
;;
(defun bury (obj n)
  (cond ((zerop n) obj)
	(t (bury (list obj) (1- n)))) )

;;;
;;;    8.49
;;;
(defun pairings (l1 l2)
  (cond ((or (null l1) (null l2)) ())
	(t (cons (list (first l1) (first l2))
		 (pairings (rest l1) (rest l2)))) ) )

(defun pairings (l1 l2)
  (mapcar #'list l1 l2))

;;;
;;;    8.50
;;;
(defun sublists (l)
  (cond ((null l) ())
	(t (cons l (sublists (rest l)))) ))

(defun sublists (l)
  (maplist #'identity l))

;;;
;;;    8.51
;;;
(defun my-reverse (l)
  (my-reverse-aux l ()) )

(defun my-reverse-aux (in out)
  (cond ((null in) out)
	(t (my-reverse-aux (rest in)
			   (cons (first in) out)))) )

;;;
;;;    8.52 see 8.34
;;;

;;;
;;;    8.53
;;;    (All elements assumed to be nonnegative integers.)
(defun largest-even (num-list)
  (cond ((null num-list) 0) ;This result is specified in the exercise.
	((oddp (first num-list)) (largest-even (rest num-list)))
	(t (max (first num-list) (largest-even (rest num-list)))) ) )

(defun largest-even (num-list)
  (apply #'max (or (remove-if-not #'evenp num-list) (list 0))))

;;;
;;;    8.54
;;;
(defun huge (n)
  (huge-aux n n) )

(defun huge-aux (m n)     ;AKA 'expt'
  (cond ((zerop n) 1)
	(t (* m (huge-aux m (1- n)))) ) )

(defun huge (n)
  (reduce #'* (make-list n :initial-element n)))

;;;
;;;    8.56
;;;
(defun every-other (l)
  (cond ((null l) nil)
;	((null (cdr l)) l)   ;This line is actually superfluous since
			     ;(cdr nil) is still nil.
	                     ; I.e., it doesn't matter if we call cddr below on
			     ; a list with 1 element.
	(t (cons (car l) (every-other (cddr l)))) ) )

;;;
;;;    8.57
;;;
(defun left-half (l)
  (left-half-aux l (ceiling (length l) 2)) )

;;
;;    Return a list containing the first n elements of a given list.
;;    (We assume that l has AT LEAST n elements.)
;;
(defun left-half-aux (l n)
  (cond ((zerop n) ())
	(t (cons (first l) (left-half-aux (rest l) (1- n)))) ) )

;;;
;;;    Now we don't have to worry about how many elements L has.
;;;    
(defun left-half (l)
  (labels ((left-half-aux (l n)
	     (cond ((zerop n) ())
		   (t (cons (car l) (left-half-aux (cdr l) (1- n)))) )))
    (left-half-aux l (ceiling (length l) 2))))

;;;
;;;    Another approach
;;;
(defun left-half (l)
  (labels ((left-half-aux (l m n)
	     (cond ((> n m) ())
		   (t (cons (car l) (left-half-aux (cdr l) (1- m) (1+ n)))) )))
    (left-half-aux l (length l) 0)))

;;;
;;;    8.58
;;;    (The input lists are assumed to be ordered.)
; (defun merge-lists (l1 l2)
;   (cond ((null l1) l2)
; 	((null l2) l1)
; 	((< (first l1) (first l2))
; 	 (cons (first l1) (merge-lists (rest l1) l2)))
; 	(t (cons (first l2) (merge-lists l1 (rest l2)))) ) )

;;;
;;;    The above version does not ensure stable sort. This does:
;;;    
(defun merge-lists (l1 l2)
  (cond ((null l1) l2)
	((null l2) l1)
	((< (first l2) (first l1))
	 (cons (first l2) (merge-lists l1 (rest l2))))
	(t (cons (first l1) (merge-lists (rest l1) l2)))) )

;;;
;;;    8.60 See genealogy.lsp
;;;

;;;
;;;    8.61
;;;
(defun count-up-tr (n)
  (count-up-tr-aux n nil) )

(defun count-up-tr-aux (n result)
  (cond ((zerop n) result)
	(t (count-up-tr-aux (1- n) 
			    (cons n result)))) )

;;;
;;;    8.62
;;;
(defun fact-tr (n)
  (fact-tr-aux n 1) )

(defun fact-tr-aux (n result)
  (cond ((zerop n) result)
	(t (fact-tr-aux (1- n)
			(* n result)))) )


;;;
;;;    8.63 -- Tail-recursive UNION, INTERSECTION, SET-DIFFERENCE
;;;            (Again, notice the symmetry)
;;;            (In fact, UNION-TR-AUX and SET-DIFFERENCE-TR-AUX are identical!!)
;;;

;;
;;    This definition is probably the most straightforward, but the
;;    second one retains the symmetry of the intersection-tr and
;;    set-difference-tr definitions.
;;
; (defun union-tr (a b)
;   (cond ((null a) b)
; 	((member (first a) b) (union-tr (rest a) b))
; 	(t (union-tr (rest a) (cons (first a) b)))) )

(defun union-tr (a b)
  (union-tr-aux a b b) )     ;<----

(defun union-tr-aux (a b result)
  (cond ((null a) result)
	((member (first a) b)
	 (union-tr-aux (rest a) b result))
	(t (union-tr-aux (rest a) b (cons (first a) result)))) )

(defun intersection-tr (a b)
  (intersection-tr-aux a b nil) )

(defun intersection-tr-aux (a b result)
  (cond ((null a) result)
	((member (first a) b)
	 (intersection-tr-aux (rest a) b (cons (first a) result)))
	(t (intersection-tr-aux (rest a) b result))) )

(defun set-difference-tr (a b)
  (set-difference-tr-aux a b nil) )     ;<----

(defun set-difference-tr-aux (a b result)
  (cond ((null a) result)
	((member (first a) b)
	 (set-difference-tr-aux (rest a) b result))
	(t (set-difference-tr-aux (rest a) b (cons (first a) result)))) )


;;;
;;;     8.64
;;;
(defun tree-find-if (f tree)
  (cond ((null tree) nil)
	((atom tree) (and (funcall f tree) tree))
	(t (or (tree-find-if f (first tree))
	       (tree-find-if f (rest tree)))) ) )


;;;
;;;     8.65
;;;
; (defun tr-count-slices (loaf)
;   (tr-count-slices-aux loaf n) )

; (defun tr-count-slices-aux (loaf count)
;   (cond ((null loaf) count)
; 	(t (tr-count-slices-aux (rest loaf) (1+ count)))) )

(defun tr-count-slices (loaf)
  (labels ((tr-count-slices-aux (loaf count)
	     (cond ((null loaf) count)
		   (t (tr-count-slices-aux (rest loaf) (1+ count)))) ))
    (tr-count-slices-aux loaf 0)) )

; (defun tr-reverse (l)
;   (tr-reverse-aux l nil) )

; (defun tr-reverse-aux (l result)
;   (cond ((null l) result)
; 	(t (tr-reverse-aux (rest l) (cons (first l) result)))) )

(defun tr-reverse (l)
  (labels ((tr-reverse-aux (l result)
	     (cond ((null l) result)
		   (t (tr-reverse-aux (rest l) (cons (first l) result)))) ))
    (tr-reverse-aux l nil)) )

;;;
;;;    8.66
;;;
(defun arith-eval (expr)
  (cond ((numberp expr) expr)
	(t (funcall (second expr) 
		    (arith-eval (first expr))
		    (arith-eval (third expr)))) ) )

;;;
;;;    8.67
;;;
;;    This version causes a core dump in clisp with (legalp nil)!!!
; (defun legalp (expr)
;   (cond ((numberp expr) t)
; 	(t (and (legalp (first expr))
; 		(member (second expr) '(+ - * /))
; 		(legalp (third expr)))) ) )

(defun legalp (expr)
  (cond ((numberp expr) t)
	((atom expr) nil)
	(t (and (= (length expr) 3)     ;Without this test, (2 + 3 k) qualifies as legal!
		(legalp (first expr))
		(member (second expr) '(+ - * /))
		(legalp (third expr)))) ) )

;;;
;;;    8.68
;;;
(defun factors (n)
  (factors-help n 2) )

(defun factors-help (n p)
  (cond ((= n 1) nil)
	((zerop (rem n p))
	 (cons p (factors-help (/ n p) p)))  ;A factor--check for multiple instances.
	(t (factors-help n (1+ p)))) )       ;Not a factor--check next integer.

(defun factors (n)
  (labels ((factors-aux (n p result)
	     (cond ((= n 1) (nreverse result))
		   ((zerop (rem n p))
		    (factors-aux (/ n p) p (cons p result)))
		   (t (factors-aux n (1+ p) result)))) )
    (factors-aux n 2 ())))

(defun factor-tree (n)
  (factor-tree-aux n 2) )

(defun factor-tree-aux (n p)
  (cond ((= 1 n) nil)
	((= n p) n)
	((zerop (rem n p))
	 (list n p (factor-tree-aux (/ n p) p)))
	(t (factor-tree-aux n (1+ p)))) )

(defun factor-tree (n)
  (labels ((factor-tree-aux (n p)
	     (cond ((= n p) n)
		   ((zerop (rem n p))
		    (list n p (factor-tree-aux (/ n p) p)))
		   (t (factor-tree-aux n (1+ p)))) ))
    (if (= n 1)
	nil
	(factor-tree-aux n 2))))

