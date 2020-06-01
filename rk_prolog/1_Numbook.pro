domains
NAME = string
CARNAME = string
CARNUMBER = string
BANK = string
MONEY = integer
NUMBER = string
CHOOSE = integer

predicates
nondeterm car(NAME, CARNAME, CARNUMBER).
nondeterm deposit(NAME, BANK, MONEY).
nondeterm person(NAME, NUMBER).
nondeterm personsname(NAME, NUMBER).
nondeterm personsnumber(NAME, NUMBER).
nondeterm switch(CHOOSE).
nondeterm menu.

clauses
person("alex","8-925-227-71-73").
person("ruby","8-925-227-71-73").
person("roma","8-921-217-51-70").
person("petr","8-925-257-24-13").
person("semen","8-925-211-11-13").
person("evgen","8-915-217-77-73").

car("inna", "AUDI", "M111AC").
car("alex", "BMV", "A121OK").
car("roma", "WT", "A180DM").

deposit("anna", "BANK1", 1000).
deposit("max", "BANK1", 200).
deposit("semen", "BANK1", 115).

personsname(NAME, NUMBER):-person(_,NUMBER).
personsnumber(NAME, NUMBER):-person(NAME,_). 


menu:- write("Hello!"), nl, 
write("1: search by user name"), nl,
write("2: search by user number"), nl,
write("0: close menu"), nl,
readint(CHOOSE),switch(CHOOSE).

switch(0):-!.
switch(1):-write("Input name:"), nl, readln(NAME),
 personsname(NAME, NUMBER), write(NAME, " : ", NUMBER), nl,menu.
switch(2):- write("Input number:"), nl, readln(NUMBER),
 personsnumber(NAME, NUMBER), write(NAME, " : ", NUMBER), nl, menu.
switch(3):-write("Extra, input name: "), readln(NAME), person(NAME, _), car(NAME, _, _).


goal
menu.
/*person(Name, _), car(Name, _, _).*/