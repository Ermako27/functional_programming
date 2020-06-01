; 1. Все числовые элементы исходного смешанного списка удвоить, 
; если сумма его первых двух числовых элементов больше 10,
; и уменьшить на 10 в противном случае.
; 2. Даны два структурированных смешанных списка. Получить из этих
; списков числовые множества (одноуровневые списки) и найти 
; пересечение этих двух множеств.
; в обеих задачах использовать функционалы; использовать рекурсию

(defun myappend (lst1 lst2)
	(
		if lst1 
			(
				cons 
					(car lst1) 
					(myappend (cdr lst1) lst2)
			) 
		lst2
	)
)

(defun tolist (lst)
	(
		cond 
			((null lst) nil)
			(
				(atom(car lst)) 
					(
						myappend 
							(list (car lst)) 
							(tolist (cdr lst))
					)
			)
			(
				(listp lst) 
					(
						myappend 
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
				(numberp (car lst))
					(myappend 
						(list (car lst)) 
						(onlynum (cdr lst))
					)
			)
			(t (onlynum (cdr lst)))
	)
)

(defun morethan (lst)
	(> (+ (car (onlynum lst)) (cadr (onlynum lst))) 10)
)

(defun f1 (lst)
	(
		if (not (null lst))
			(
				if (morethan lst)
					(tolist
						(
							mapcar #'
								(
									lambda (x)
										(
											cond
												((numberp x) (list x x))
												(t x)
										)
								
									
								)
								lst
						)
					)
						(
							mapcar #'
								(
									lambda (x)
										(
											cond
												((numberp x) (- x 10))
												(t x)
										)
								
									
								)
								lst
						)
			)
	)
)

(defun intersect (s1 s2)
	(
		if (and s1 s2)
			(
				remove nil
					(
						mapcar #'
							(
								lambda (x)
									(
										cond
											((member x s2) x)
											(t nil)
									)
							
							)
							s1
					)
			)
	)
)

(defun toset (lst)	
	(
		if lst
			(
				cond
					((numberp (car lst))
						(
							if (member (car lst) (cdr lst))
								(toset (cdr lst))
									(myappend (list (car lst)) (toset (cdr lst))) 
						)
							
							
					)
					(t 
						(toset (cdr lst))					
					)

			)
	)
)

(defun f2 (lst1 lst2)
	(
		intersect(toset(tolist lst1)) (toset(tolist lst2)) 
	)
)

