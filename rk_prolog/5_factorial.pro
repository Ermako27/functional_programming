domains 
num = integer

predicates

nondeterm factorial_a(num, num)
nondeterm factorial_b(num, num)
nondeterm f(num, num, num, num)
  

clauses

  factorial_a(X, _):- X < 0, !.
  factorial_a(0, 1):- !.
  factorial_a(X, F):- TMP = X-1, factorial_a(TMP, F_tmp), F = F_tmp * X.
  
  factorial_b( X, F):- f( X, F, 1, 1).
  f(X, F, X, F):- !.
  f(X, F, X1, F1):- TMP = X1 +1, F_TMP = F1* TMP, f(X, F, TMP, F_TMP).
  
goal

  factorial_b(3, A).