; Author: Julien Hoachuck

; Part 1-----
; double-recursive(Y)
(defun double-recursive (Y)
	(if (atom Y) nil (append  (list (car Y)) (list (car Y)) (double-recursive (cdr Y))))
	)
; double-iterative
(defun double-iterative (Y)
	(do ((temp (if (atom Y) nil Y) (cdr temp)) (result nil (append result (list (car temp)) (list (car temp)) )))
		((null temp) result)
	)
	)
; double-mapcar
(defun double-mapcar (Y) 
		(if (atom Y) nil (apply 'append (mapcar 'list Y Y))) '(1 2 3)
		;(cond
		;((atom Y) nil)
		;(t (apply 'append (mapcar 'car Y))))
	)

; Part 2-----
; rdouble (y)
(defun rdouble (Y)
	(cond 
		((atom Y) nil)
		((listp (car Y)) (append (list (rdouble(car Y))) (rdouble(cdr Y))) ) ; must return the whole append() ... last evaluated value
		(t (append (list (car Y)) (list (car Y)) (rdouble (cdr Y))))
		)
	)

; Part 3-----
; my-mapcar(fun l1 l2)
(defun my-mapcar (fun l1 l2)
	(cond
		((or (null l1) (null l2)) nil)
		(t (append (list (funcall fun (car l1) (car l2))) (my-mapcar fun (cdr l1) (cdr l2))))
		)
		)

; Part 4-----
; rewrite(x)
(defun rewrite(x)
	(cond
		((atom x) x)
		((and (atom (car x)) (equal (car x) 'if))  (append (list 'cond)  
			(list (list (if (listp (cadr x)) (rewrite (cadr x)) (cadr x)) (if (listp (caddr x)) (rewrite (caddr x)) (caddr x))))
			 (if (not(null (cadddr x))) (list (list 't (if (listp (cadddr x)) (rewrite (cadddr x)) (cadddr x)))) ) ))   
		;((and (listp (car x)) (equal (caar x) 'if)) (list (append (list 'cond)  (list (list (cadar x) (caddar x))) (if (not(null (cadddr (car x)))) (list (list 't (cadddr(car x))))))))
		((and (listp (car x)) (equal (caar x) 'if)) (append (list (rewrite(car x))) (rewrite(cdr x))))
		(t (append (list (car x)) (rewrite (cdr x))) ) 

	)
	)

; Part 5-----
; check(x)
(defun check(x)
	(append (list (equal (eval x) (eval (rewrite x)) )) (list (eval x)) (list (eval (rewrite x))) )
	)



