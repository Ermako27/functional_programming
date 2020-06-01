domains

result = real
operation = string
tree = t(operation, tree, tree); v(real).

predicates

count(tree, result).

clauses
  
  count(t("+", LEFT, RIGHT), Res):- count(LEFT, R1), count(RIGHT, R2), Res = R1 + R2.
  count(t("-", LEFT, RIGHT), Res):- count(LEFT, R1), count(RIGHT, R2), Res = R1 - R2.
  count(t("*", LEFT, RIGHT), Res):- count(LEFT, R1), count(RIGHT, R2), Res = R1 * R2.
  count(t("/", LEFT, RIGHT), Res):- count(LEFT, R1), count(RIGHT, R2), R2 <> 0,  Res = R1 / R2.
  count(v(X), X):-!.
  

goal

  %ount(t(X, v(5), t(Y, t(Z, v(2) ,v(3)), v(6))), 5).
  count(t("+", v(5), t("-", v(6), t("*", v(2) ,v(3)))), X).
