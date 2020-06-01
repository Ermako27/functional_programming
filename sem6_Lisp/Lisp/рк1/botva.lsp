;1) преобразовать структурированный смешанный список в числовой одноуровневый
;2) Все числовые элементы одноуровнего смешанного списка уменьшить на K, 
;если его первый числовой элемент больше 10, после чего все нечетные элементы умножить на -1 (функционалы и рекурсия)

(defun tolist (lst)
	(
		cond 
			((null lst) nil)
			(
				(atom(car lst)) 
					(
						append 
							(list (car lst)) 
							(tolist (cdr lst))
					)
			)
			(
				(listp lst) 
					(
						append 
							(tolist (car lst)) 
							(tolist (cdr lst))
					)
			)
	)
)

(defun onlynum (lst)
	(
		cond 
			((null lst) nil)
			(
				(numberp(car lst)) 
					(
						append 
							(list (car lst)) 
							(onlynum (cdr lst))
					)
			)
			(
				t 
					(onlynum (cdr lst))
			)
	)
)


(defun f1 (lst)
	(onlynum (tolist lst))
)

(defun f-multodd (lst)
	(
		mapcar 
			(function
				(lambda (x)
					(
							cond 
								((and (numberp x) (oddp x)) (* (- 1) x) )
								(t x)
					)
				)
			)
			lst
	)
)

(defun f-less (lst k)
	(
		mapcar 
			(function
				(lambda (x)
					(
							cond 
								((numberp x) (- x k))
								(t x)
					)
				)
			)
			lst
	)
)

(defun f-findfirstnumber (lst)
	(
		mapcar 
			(function
				(lambda (x)
					(
							cond 
								((and (numberp x) (> x 10)) t )
								(t x)
					)
				)
			)
			lst
	)
)

(defun f-f2 (lst k)
	(
		if 
			(not (equal (position t (f-findfirstnumber lst)) nil))
				(f-multodd (f-less lst k))
					(f-multodd lst)

	)	
)

(defun r-findfirstnumber (lst)
	(							
		cond 
			((null lst) nil)
			((and (numberp (car lst)) (> (car lst) 10)) t )
			(t (findfirstnumber (cdr lst)))
	)
)

(defun r-less (lst k)
	(							
		cond 
			((null lst) nil)
			((numberp (car lst)) (append (list (- (car lst) k)) (r-less (cdr lst) k)))
			(t (append (list (car lst)) (r-less (cdr lst) k)))
	)
)

(defun r-multodd (lst)
	(							
		cond 
			((null lst) nil)
			((and (numberp (car lst)) (oddp (car lst))) (append (list (* (car lst) (- 1))) (r-multodd (cdr lst))))
			(t (append (list (car lst)) (r-multodd (cdr lst))))
	)
)


(defun r-f2 (lst k)
	(
		if 
			(r-findfirstnumber lst)
				(r-multodd (r-less lst k))
					(r-multodd lst)

	)	
)