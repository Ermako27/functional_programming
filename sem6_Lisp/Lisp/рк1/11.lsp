;11
;1 - вычислить n!! n - количество чисел
(defun n_count_z1 (lst)
	(reduce #'+
		(mapcar #'(lambda (x) 
				(if (numberp x)
					1
					0
				)
			)
		lst)
	)
)

(defun calc_z1 (n res)
	(if (< n 1)
		res
		(calc_z1 (- n 2) (* res n))
	)
)

(defun f11_z1 (lst)
	(calc_z1 (n_count_z1 lst) 1)
)

(f11_z1 '(1 q 2 e 5 g 3 fd)) ;-> 8

;2 - два смешанных множества, оставить только числа, вычислить объединение
(defun del_word_z2 (lst)
	(remove nil 
		(mapcar #'(lambda (x) 
				(if (numberp x)
					x
				)
			)
		lst)
	)
)

(defun union_z2 (lst1 lst2)
	(cond ((null lst1) lst2)
		((null lst2) lst1)
		((member (car lst1) lst2) (union_z2 (cdr lst1) lst2))
		(t (cons (car lst1) (union_z2 (cdr lst1) lst2)))	
	)
)

(defun f11_z2 (lst1 lst2)
	(union_z2 (del_word_z2 lst1) (del_word_z2 lst2))
)

(f11_z2 '(1 q 2 e 5 g 3 fd) '(3 d 2 s 5 g 1 vc 6 gf 4)) ;-> (3 2 5 1 6 4)
