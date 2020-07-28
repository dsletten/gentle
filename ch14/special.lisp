;;;
;;;    Version 1
;;;    Why are all of these DECLAREs (besides the first one) "supposed" to be
;;;    here?
;;;    
(let ((fish '(salmon tuna))
      (birds '(eagle vulture)))
  (declare (special birds))

  (defun ref-fish ()
    fish)

  (defun ref-birds () ;Will not work if called from top-level! Compare below.
    (declare (special birds))
    birds)

  (defun test-lexical (fish)
    (list fish (ref-fish)))

  (defun test-dynamic (birds)
    (declare (special birds))
    (list birds (ref-birds))))

;;;
;;;   Version 2
;;;   Equivalent to version 1. BIRDS is special, so no closure is created by
;;;   REF-BIRDS. It can be moved outside the LET.
;;;   
(let ((fish '(salmon tuna))
      (birds '(eagle vulture)))
  (declare (special birds))

  (defun ref-fish ()
    fish)

  (defun test-lexical (fish)
    (list fish (ref-fish))))

(defun ref-birds ()
  (declare (special birds))
  birds)

(defun test-dynamic (birds)
  (declare (special birds))
  (list birds (ref-birds)))

;;;
;;;    Version 3
;;;    It doesn't matter whether or not TEST-LEXICAL is inside the LET. The
;;;    FISH parameter of TEST-LEXICAL is a lexical variable irrespective of the
;;;    scope of the FISH created by the LET.
;;;    
(defvar birds '(eagle vulture))
(defun ref-birds () ; Now this works at top-level.
  birds)

(let ((fish '(salmon tuna)))
  (defun ref-fish ()
    fish))

(defun test-dynamic (birds)
  (list birds (ref-birds)))

(defun test-lexical (fish)
  (list fish (ref-fish)))