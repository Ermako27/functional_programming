domains
num = integer

predicates

nondeterm fib(num, num)

clauses

fib(1, 1):- !.
fib(2, 1):- !.
fib(X, R):- P1 = X-1, P2 = X-2, fib(P1, R1), fib(P2, R2), R = R1 + R2.


goal

  fib(5, R).