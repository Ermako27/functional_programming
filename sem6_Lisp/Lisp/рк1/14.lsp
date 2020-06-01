;14
;1- ассоциативная таблица. ключи - числа. 
;	выделить нечетные, из ключей вычесть размер рез списка
(defun mk_z1 (lst)
	(if (not (null lst))
		(cons (car lst) (mk_z1 (cddr lst)))
	)
)

(defun length_z1 (lst) 
	(do ((x lst (cdr x)) (i 0 (+ i 1)))
		(( null x) i )
	)
)

(defun f14_z1 (lst &aux (new_lst (mk_z1 lst)))
	(mapcar #'(lambda (x) (cons (- (car x) (length_z1 new_lst)) (cdr x)))
	new_lst)
)


(f14_z1 '((1 a) (2 b) (3 c) (4 d) (5 e))) ;-> ((-2 a) (0 c) (2 e))

;2- из смешанного списка выделить в отдельные списки числа, 
;	лежащие в заданном интервале, и символы

(defun list-if-not_z2 (pred) 
	(lambda (x) (if ( funcall pred x) nil (list x)))
)

(defun remove-if_z2 (pred lst) 
	(mapcan (list-if-not_z2 pred) lst)
)


(defun for_numb_z2 (lst a b) 
	(remove-if_z2 #'
		(lambda (x)
			(or (<= x a) (>= x b ))
		)
	lst)
)

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

(defun del_num_z2 (lst)
	(remove nil 
		(mapcar #'(lambda (x) 
				(if (not (numberp x))
					x
				)
			)
		lst)
	)
)

(defun f14_z2 (lst a b &aux (res1 (del_word_z2 lst)) (res2 (del_num_z2 lst)))
	(format t "Numbers from list: ~S~%" (for_numb_z2 res1 a b)) 
	(format t "Symbols from list: ~S~%" res2) 
)

(f14_z2 '(q 8 w 1 e 7 r 3 t y 5 l 6 9 o l 4) 2 8) ;-> Numbers from list: (7 3 5 6 4)
													; Symbols from list: (Q W E R T Y L O L)
