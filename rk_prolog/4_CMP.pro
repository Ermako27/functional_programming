domains
num = integer

predicates

nondeterm max_of2a(num, num, num).
nondeterm max_of2b(num, num, num).

nondeterm max_of3a(num, num, num, num).
nondeterm max_of3b(num, num, num, num).

clauses

max_of2a(A, B, A):- A >= B.
max_of2a(A, B, B):- B > A.

max_of2b(A, B, A):- A >= B.
max_of2b(_, B, B). 

max_of3a(A, B, C, A):- A >= B, A >= C.
max_of3a(A, B, C, B):- B >= A, B >= C.
max_of3a(A, B, C, C):- C >= B, C >= A.

max_of3b(A, B, C, A):- A >= B, A >= C, !.
max_of3b(_, B, C, B):- B >= C, !.
max_of3b(_, _, C, C).


goal

  max_of2b(3, 1, Z).