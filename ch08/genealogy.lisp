;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               genealogy.lisp
;;;
;;;   STARTED:            Fri Jul  5 21:41:42 2002
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

(defconstant family '((colin nil nil)
		      (deirdre nil nil)
		      (arthur nil nil)
		      (kate nil nil)
		      (frank nil nil)
		      (linda nil nil)
		      (suzanne colin deirdre)
		      (bruce arthur kate)
		      (charles arthur kate)
		      (david arthur kate)
		      (ellen arthur kate)
		      (george frank linda)
		      (hillary frank linda)
		      (andre nil nil)
		      (tamara bruce suzanne)
		      (vincent bruce suzanne)
		      (wanda nil nil)
		      (ivan george ellen)
		      (julie george ellen)
		      (marie george ellen)
		      (nigel andre hillary)
		      (frederick nil tamara)
		      (zelda vincent wanda)
		      (joshua ivan wanda)
		      (quentin nil nil)
		      (robert quentin julie)
		      (olivia nigel marie)
		      (peter nigel marie)
		      (erica nil nil)
		      (yvette robert zelda)
		      (diane peter erica)))

(defun father (person)
  (let ((entry (assoc person family)))
    (if entry
	(second entry)
	nil)))
;	`(no entry for ,person))))

(defun mother (person)
  (let ((entry (assoc person family)))
    (if entry
	(third entry)
	nil)))
;	`(no entry for ,person))))

(defun parents (person)
  (let ((entry (assoc person family)))
    (if entry
	(remove nil (rest entry))
	nil)))
;	`(no entry for ,person))))

(defun fatherp (father child)
  (and father child (eq father (father child))))

(defun motherp (mother child)
  (and mother child (eq mother (mother child))))

(defun parentp (parent child)
  (cond ((null parent) nil)
	((null child) nil)
	((fatherp parent child) t)
	((motherp parent child) t)
	(t nil)))

(defun parentp (parent child)
  (and parent child (or (fatherp parent child)
			(motherp parent child))))

(defun children (person)
  (labels ((get-children (f result)
	     (cond ((null f) (nreverse result))
		   ((parentp person (caar f))
		    (get-children (cdr f) (cons (caar f) result)))
		   (t (get-children (cdr f) result)))) )
    (if (assoc person family)
	(get-children family ())
	nil)))
;	`(no entry for ,person))))

;;;
;;;    A version Graham would be proud of.
;;;    
(defun children (person)
  (if (assoc person family)
      (let ((result ()))
	(dolist (entry family)
	  (when (parentp person (car entry))
	    (push (car entry) result)))
	(nreverse result))
      nil))
;      `(no entry for ,person)))

(defun siblings (person)
  (remove person (union (children (father person))
			(children (mother person)))) )

(defun mapunion (fn l)
  (and l
       (reduce #'union (mapcar fn l))))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(defun cousins (person)
  (mapunion #'children (append (siblings (father person))
			       (siblings (mother person)))) )

(defun cousins (person)
  (mapunion #'children (mapunion #'siblings (parents person))))

(defun childp (child person)
  (and child person (member child (children person))))

(defun siblingp (sibling person)
  (and sibling person (member sibling (siblings person))))

(defun grandparentp (grandparent person)
  (and grandparent person (member grandparent (grandparents person))))

(defun cousinp (cousin person)
  (and cousin person (member cousin (cousins person))))

;;;
;;;    Bottom up
;;;    
(defun descended-from (person ancestor)
  (cond ((null person) nil)
	((null ancestor) nil)
	((parentp ancestor person) t)
	((descended-from (father person) ancestor) t)
	((descended-from (mother person) ancestor) t)
	(t nil)))

(defun descended-from (person ancestor)
  (and person ancestor
       (or (parentp ancestor person)
	   (descended-from (father person) ancestor)
	   (descended-from (mother person) ancestor))))

;;;
;;;    Top down
;;;    (First version may do more work than is necessary. It may continue
;;;    searching to build the list by MAPCAR even though a true result already
;;;    exists in the list. E.g.:
;;;      (trace descended-from)
;;;      (descended-from 'yvette 'linda))
;;;      
(defun descended-from (descendant person)
  (and person descendant
       (or (childp descendant person)
	   (notevery #'null (mapcar #'(lambda (person)
					(descended-from descendant person))
				    (children person)))) ))

(defun descended-from (descendant person)
  (and person descendant
       (or (childp descendant person)
	   (descended-list descendant (children person)))) )

(defun descended-list (descendant l)
  (cond ((null l) nil)
	((descended-from descendant (car l)))
	(t (descended-list descendant (cdr l)))) )
;;;
;;;    UNION is necessary to remove duplicates. For example, Arthur and Kate
;;;    are ancestors of each of Yvette's parents!
;;;    
(defun ancestors (person)
  (let ((ancestors (parents person)))
    (if ancestors
	(append ancestors (union (ancestors (father person))
				 (ancestors (mother person))))
	nil)))

(defun generation-gap (person ancestor)
  (cond ((not (descended-from person ancestor)) nil)
	((parentp ancestor person) 1)
	(t (1+ (or (generation-gap (father person) ancestor)
		   (generation-gap (mother person) ancestor)))) ))
