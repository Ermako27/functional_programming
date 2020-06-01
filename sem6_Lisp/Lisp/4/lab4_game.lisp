(defun move() (+ (random 6) 1))

(defun move2() (list (move) (move))) 

(defun sum(x) (apply '+ x))

(defun check (x num)
	(and (= (car x) num) (= (cadr x) num)))

(defun double(x)
	(or (check x 1) (check x 6)))

(defun rep(x)
	(if (double x)
		(turn(move2))
		x))

(defun win_sum(x) 
	(or (= (sum x) 7) (= (sum x) 11) ))

(defun win_check(pl)
	(if (win_sum pl)
		(list "WIN WITH " (sum pl) ) ) )

(defun who_win(pl1 pl2)
	(cond
		((> (sum pl1) (sum pl2) ) (list "FIRST WIN WITH "  (sum pl1) ) )
		((< (sum pl1) (sum pl2) ) (list "SECOND WIN WITH " (sum pl2) ) )
		(T "friendship wins")))

(defun game () 
	(let ((pl1 '(4 5)) (pl2 (rep (move2))))		
	(print pl1)
	(print pl2)
	(print (or (win_check pl1)
		(win_check pl2)
		(who_win pl1 pl2) ))))
	
(game)