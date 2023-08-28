:- module(algorithms).

init_count_list(Length, List):-
  length(List, Length),
  maplist(=(0), List).

increment_count(Unsorted, CountList, NewCountList).
increment_count([H|T], CountList, NewCountList):-
  nth0(H, CountList, Val),  
  NewVal is Val + 1,
  replace_nth().

replace_nth()

counting_sort(Unsorted, Sorted):-
  length(Unsorted, Len),
  init_count_list(Len, CountList),


