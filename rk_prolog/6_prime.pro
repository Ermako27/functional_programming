domains
i = integer
list = i*

predicates

divisible(i, i)
isprime(i)
append(list, list, list)

list_prime_b(list, list)

list_prime(list, list, list)
reverse_list(list, list)
reverse(list, list, list)

clauses

reverse_list(List, Result):-
reverse(List, [], Result).

reverse([], Result, Result):-!.
reverse([H|T], List, Result):-
reverse(T, [H|List], Result).
   
divisible(X,Y):-
N = Y*Y,
N <= X,
X mod Y <> 0.

divisible(X,Y):-
Y = X,
Y1 = Y+1,
divisible(X,Y1).

isprime(X):-
Y = 2, X > 1, divisible(X,Y).

append([], L, L). 
append([H|T], L2, [H|L3]):- append(T, L2, L3).

list_prime([], Res, Res):- !.
list_prime([H|T], Buf, Res):- isprime(H), !, list_prime(T,[H|Buf], Res).
list_prime([_|T], Buf, Res):- list_prime(T, Buf, Res).

list_prime_b([], Res):-!.
list_prime_b([H|T], [H|Res]):- isprime(H), !, list_prime_b(T, Res).
list_prime_b([_|T], Res):- list_prime_b(T, Res).

goal
  list_prime_b([5,7,11], Res).