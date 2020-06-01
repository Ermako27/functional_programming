domains
i = integer
list = i*
predicates
len(list, integer)
sum(list, integer)
delete(list, integer, list)
delete_once(list, integer, list)
sub_delete(list, integer, integer, list)
append(list, list, list)
append_more(list, integer, list)
append_even(list, integer, list)
make_set(list, list)
is_member(list, integer)
con(list, list, list)
clauses	
len([], 0).
len([_|T], COUNT):- len(T, TAIL_COUNT), COUNT = TAIL_COUNT + 1.

sum([], 0).
sum([H | T], SUM):- sum(T, TAIL_SUM), SUM = TAIL_SUM + H.


delete_once(SRC, INT, RES):-
    sub_delete(SRC, INT, 0, RES).
sub_delete([], _, _ ,[]).
sub_delete([H|T], INT, 0, RES):-
    H=INT,sub_delete(T, INT, 1, RES),!.
sub_delete([H|T], INT, MODE, [H|RES]):-
    sub_delete(T, INT, MODE, RES).

delete([], _, []).
delete([H|T], INT, RES):-
    H=INT,delete(T, INT, RES),!.
delete([H|T], INT, [H|RES]):-
    delete(T, INT, RES),!.
    
    
append([], LIST2, LIST2).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

append_more([], INT, []).
append_more([H|L1], INT, [H|RES]):- H>INT, append_more(L1, INT, RES),!.
append_more([H|L1], INT, RES):- append_more(L1, INT, RES).

append_even([], INT, []).
append_even([H|L1], 0, [H|RES]):- append_even(L1, 1, RES),!.
append_even([H|L1], 1, RES):- append_even(L1, 0, RES).

is_member([El|_], El):-
    !.
is_member([_|Tail], El):-
        is_member(Tail,El).
        
    make_set([],[]).
    make_set([H|T], SET):-
        make_set(T, SET),is_member(SET, H),!.
    make_set([H|T], [H|SET]):-
        make_set(T, SET).
        
    con([], L2, L2).
    con(L1, [], L1).
    con(L1, L2, L3):- append(L1, L2, L3),len(L1, N1),len(L2, N2),N1<N2,!.
    con(L1, L2, L3):- append(L2, L1, L3).
    
    
goal
delete_once([1, 2, 3, 4, 5, 3], 3, RES).