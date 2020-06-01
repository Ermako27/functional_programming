;10
;1 - разность двух множеств
(defun memberp (elem lst ) 
	(dolist ( el lst )
		(if (equal elem el) 
			(return T) 
		) 
	) 
)
 
(defun f10_z1 (x y)
	(cond 	((null x) nil)
			((null y) x)
			((memberp (car x) y) (f10_z1 (cdr x) y))
			(t (cons (car x) (f10_z1 (cdr x) y)))
	)
)

(f10_z1 '(1 a b 2 c 1 d a 1 1 1 e 3 f) '(a 1)) ;-> (b 2 c d e 3 f)

;2 - удалить из списка атом/другой список
(defun eqp (x y)
	(cond 	((null y) T)
			(t (and (equal (car x) (car y)) (eqp (cdr x) (cdr y))))
	)
)

(defun cut (n lst)
	(cond 	((< n 1) lst)
			(t (cut (- n 1) (cdr lst)))
	)
)

(defun l-diff (lst el &aux (a (car lst)) (d (cdr lst)))
	(cond ((null lst) nil)
		((listp a) 
				(cons (l-diff  (car lst) el) (l-diff  d el))
			)
		((eqp lst el) (f10_z2 (cut (length el) lst) el))
		(t (cons a (f10_z2 d el)))
	)
)

(defun at-diff (lst el &aux (a (car lst)) (d (cdr lst)))
	(cond 	((null lst) nil)
			((listp a) 
				(cons (at-diff (car lst) el) (at-diff d el))
			)
			((equal a el) (at-diff d el))
			((cons a (at-diff d el)))
	)
)

(defun f10_z2 (lst el)
	(cond 	((atom el) (remove nil (at-diff lst el)))
			((listp el) (remove nil (l-diff lst el)))
	)
)



(f10_z2 '(1 e 3 (w 1 e 4) q ((1 e w))) '(1 e)) ;-> (3 (w 4) q ((w)))
(f10_z2 '(1 e 3 (w 1 e 4) q ((1 e w))) '1) ;-> (e 3 (w e 4) q ((e w)))