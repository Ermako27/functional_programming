;; равен ли первый элемент последнему
(defun first_last_equal (lst)
    (if (eql (car lst) (car (reverse lst))) T))

;; возвращает список без первого и полседнего элемента
(defun without_first_and_last (lst)
    (cdr (reverse (cdr lst))))
;; является ли список палиндромом
(defun is_palyndrom (lst) 
    (cond 
        ( (<= (length lst) 1) T )
        ( (first_last_equal lst) (is_palyndrom (without_first_and_last lst)))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; подсчет количества одного элемента в списке
(defun elem_count (elem lst)
    (cond 
        ((null lst) 0)
        ((eql (car lst) elem) (+ 1 (elem_count elem (cdr lst))))
        (t (+ 0 (elem_count elem (cdr lst))))
    )
) 

;; удалить все вхождения элемента в список
(defun remove_elem (elem lst)
    (remove-if #'(lambda (x) (eql elem x)) lst)
)

;; встречается ли каждый элемент списка четное количество раз
(defun consists_of_pairs (lst)
    (cond 
        ((null lst) T)
        ((oddp (elem_count (car lst) lst)) nil)
        (t (consists_of_pairs (remove_elem (car lst) lst) ))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; подсчет элементов, которые встречаются в списке нечетное количество раз
(defun count_of_odd_count_elements (lst) 
    (cond 
        ( (null lst) 0)
        ( (oddp (elem_count (car lst) lst)) (+ 1 (count_of_odd_count_elements (remove_elem (car lst) lst))))
        (t (+ 0 (count_of_odd_count_elements (remove_elem (car lst) lst))))
    )
)

;; удаление элементов, которые встречаются в списке нечетное количество раз
(defun remove_odd_count_element (lst) 
    (remove-if #'(lambda (x) (oddp (elem_count x lst))) lst)
)

;; можно ли из элементов списка сделать палиндром
(defun can_be_palydrom (lst)
    (cond 
        ( (evenp (length lst)) (consists_of_pairs lst))
        ( (oddp (length lst))
            (cond
                ((eql (count_of_odd_count_elements lst) 1) (consists_of_pairs (remove_odd_count_element lst)))
                (t nil)
            )
        )
    )
)

;; main
(defun create_new_list (lst)
    ;; (remove-if #'(lambda (x) (or (is_palyndrom x) (can_be_palydrom x))) lst)
    (remove-if #'can_be_palydrom lst)

)





;; количество одиночных(без повторений) элементов в списке
;; (defun count_of_single_elements (lst) 
;;     (cond 
;;         ( (null lst) 0)
;;         ( (eql (elem_count (car lst) lst) 1) (+ 1 (count_of_single_elements (remove_elem (car lst) lst))))
;;         (t (+ 0 (count_of_single_elements (remove_elem (car lst) lst))))
;;     )
;; )