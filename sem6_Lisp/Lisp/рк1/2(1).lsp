; 1. Найти А\В, где А и В - числовые множества, полученные из двух смешанных одноуровневых списков.
; 2. Дан смешанный структурированный список. Реализовать удаление из списка, не меняя его структуры,
; заданных элементов: атома и списка
; в обоих задачах использовать функционалы; использовать рекурсию
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

(defun r-f1 (s1 s2)
	(
		cond
			((null s1) (null s2))
			(
				(numberp (car s1))
					(
						if 
							(not (member (car s1) s2))
								(myappend (list (car s1)) (r-f1 (cdr s1) (remove (car s1) s2)))
									(r-f1 (cdr s1) s2)
					)
			)
			(t (r-f1 (cdr s1) s2))
	)
)

(defun f-f1 (s1 s2)
	(remove nil
		(
			if s1
			(
				mapcar #'
					(
						lambda (x)
						(
							cond
								((numberp x) 
									(
										if 
											(member x s2)
												nil
													x
													
									)
								)
								(t nil)		
						
						
						)
					) 
					s1
			)
			nil
		
		)
	)
)

(defun cmp (lst1 lst2)
	(
		cond
			((and (null lst1) (null lst2)) t)
			((and (equal (car lst1) (car lst2)) (f-cmp (cdr lst1) (cdr lst2)))t)
	)
)

(defun f-f2 (lst el)
	(if (listp el)
		(remove nil
			(
				if lst
				(
					mapcar #'
						(
							lambda (x)
							(
								cond
									((and (listp x) (cmp el x)) nil)
									((and (listp x) (equal (cmp el x) nil))
										(myappend (list (car x)) (f-f2 (cdr x) el)))
									(t x)										
							)
						) 
						lst
				)							
			)
		)
	)
		(remove nil
			(
				if lst
				(
					mapcar #'
						(
							lambda (x)
							(
								cond
									((and (atom x) (equal el x)) nil)
									(
										(listp x) 
											(
												if (equal (car x) el)
													(f-f2 (cdr x) el)
														(myappend (list (car x)) (f-f2 (cdr x) el))
											)
									)
									(t x)										
							)
						) 
						lst
				)							
			)
		)

	
)