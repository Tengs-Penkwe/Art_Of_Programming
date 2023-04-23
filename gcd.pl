gcd(X, 0, X).
gcd(0, Y, Y).

gcd(X, Y, Result) :-
  (X > Y -> 
    gcd(Y, X, Result)
  ;
    d is X div Y,
    r is X mod Y,
    gcd(d, r, Result)
  ).
