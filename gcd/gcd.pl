gcd(X, 0, X) :- !.

gcd(X, Y, Result) :-
  (Y > X -> 
    gcd(Y, X, Result)
  ;
    R is X mod Y,
    gcd(Y, R, Result)
  ).
