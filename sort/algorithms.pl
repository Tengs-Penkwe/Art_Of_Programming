:- module(algorithms, [counting_sort/2]).

% The main function for counting sort
counting_sort(Unsorted, Sorted):-
  get_count(Unsorted, Unsorted, CountList), % Get the count list of positions for each element
  length(Unsorted, Len),  % Find the length of the unsorted list
  length(Sorted, Len),  % Initialize the sorted list with the same length as unsorted
  generate_sorted_list(CountList, Unsorted, Sorted).  % Generate the sorted list

% Initialize the count list with zeros
init_count_list(Length, List):-
  length(List, Length),
  maplist(=(0), List).

% Count how many elements are <= each element in the unsorted list
get_count([], _, []).
get_count([H|T], List, [Order|Os]):-
  get_order(List, H, Order),  % Count elements that are <= H
  get_count(T, List, Os).

% Helper function for get_count
get_order([], _, 0).
get_order([H|T], Element, Order):-
  get_order(T, Element, NewOrder),
  (   Element > H
  ->  Order is NewOrder + 1
  ;   Order = NewOrder
  ).

% Generate the sorted list
generate_sorted_list([], [], _).
generate_sorted_list([Count|Counts], [Ele|Eles], Sorted):- 
  find_replicate([Count|Counts], Count, Nums),  % Find the number of replicates for this Count
  NewOrder is Count + Nums - 1,
  replace_nth(TempSorted, NewOrder, Ele, Sorted),  % Place the element at its correct position
  generate_sorted_list(Counts, Eles, TempSorted).  % Recursive call for the rest of the elements

find_replicate([], _, 0).
find_replicate([H|T], Ele, Count):-
  find_replicate(T, Ele, NewCount),
  (   Ele = H
  ->  Count is NewCount + 1
  ;   Count = NewCount
  ).

% Replace the Nth element in a list
replace_nth([_|T], 0, X, [X|T]).
replace_nth([H|T], I, X, [H|R]) :-
  I > 0,
  NewI is I - 1,
  replace_nth(T, NewI, X, R).
