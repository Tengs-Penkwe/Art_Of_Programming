
generate_random_list(N, Min, Max, List):-
  length(List, N),
  maplist(random(Min, Max), List).

random(Min, Max, X):-
  random(RandomFloat),
  Range is Max - Min + 1,
  X is Min + floor(RandomFloat * Range).


