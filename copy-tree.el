(defun copy-tree-new (tree)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
This version of `copy-tree' works correctly even if loops occur in the
chain of cons cells."
  (let ((tree-hash (make-hash-table)))
    (fset 'copy-tree-new--inner
	  (byte-compile
	   (lambda (tree)
	     (let ((hashlookup (gethash tree tree-hash)))
	       (if hashlookup
		   hashlookup
		 (let ((mycons (cons t t)))
		   (puthash tree mycons tree-hash)
		   (let ((cartree (car tree)))
		     (setcar mycons
			     (if (consp cartree) (copy-tree-new--inner cartree)
			       cartree)))
		   (let ((cdrtree (cdr tree)))
		     (setcdr mycons
			     (if (consp cdrtree) (copy-tree-new--inner cdrtree)
			       cdrtree)))
		   mycons))))))
    (copy-tree-new--inner tree)))


;; Example usage:

(require 'generator)

(iter-defun asdf ()
  (let ((x 9))
    (iter-yield 10)
    (setq mm 2)
    (iter-yield 20)
    (setq mm 3)
    (iter-yield 30)
    (iter-yield 40)))

(setq kk (asdf))

(iter-next kk)

(setq kk-copy1 (copy-tree-new kk))

(setq kk-copy2 (copy-tree-new kk))

(iter-next kk)
(iter-next kk-copy1)
(iter-next kk-copy2)
