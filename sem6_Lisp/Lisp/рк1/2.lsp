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