:- use_module('./algorithms.pl').

% Define the list of algorithms to test
algorithms_to_test([counting_sort]).

% General test runner
run_tests_for_algorithms:-
  algorithms_to_test(Algorithms),
  forall(member(Algo, Algorithms), run_sort_tests(Algo)).

% Generalized test function
run_sort(SortPredicate, UnsortedList, ExpectedSortedList) :-
    call(SortPredicate, UnsortedList, Sorted),
    assertion(Sorted = ExpectedSortedList).

:- begin_tests(sort_tests).

run_sort_tests(Algo):-
    run_sort(Algo, [4, 1, 3, 2, 5], [1, 2, 3, 4, 5]),
    run_sort(Algo, [], []),
    run_sort(Algo, [4, 4, 3, 1, 2, 2], [1, 2, 2, 3, 4, 4]),
    run_sort(Algo, [1], [1]),
    run_sort(Algo, [2, 1], [1, 2]),
    run_sort(Algo, [-3, -1, -4, -2], [-4, -3, -2, -1]),
    run_sort(Algo, [3, -1, 4, -2, 0], [-2, -1, 0, 3, 4]),
    run_sort(Algo, [1, 1, 1, 1, 1], [1, 1, 1, 1, 1]),
    run_sort(Algo, [10000, 20000, 10000], [10000, 10000, 20000]),
    run_sort(Algo, [5, 5], [5, 5]),
    run_sort(Algo, [-2, -2, -2], [-2, -2, -2]),
    run_sort(Algo, [0], [0]),
    run_sort(Algo, [5, 4, 3, 2, 1], [1, 2, 3, 4, 5]),
    run_sort(Algo, [3, 3, 2, 2, 1], [1, 2, 2, 3, 3]),
    run_sort(Algo, [1,5,3,2,4,1,5,3,2,4,1,5,3,2,4], [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5]),
    run_sort(Algo, [100, 200, 300, 100, 200, 300], [100, 100, 200, 200, 300, 300]),
    run_sort(Algo, [23, 1, 45, 67, 89, 11, 8, 109, 21, 0], [0, 1, 8, 11, 21, 23, 45, 67, 89, 109]),
    run_sort(Algo, [0, 0, 0, 0, 0, 1], [0, 0, 0, 0, 0, 1]),
    run_sort(Algo, [0, -1, -1, 0, 0, -1], [-1, -1, -1, 0, 0, 0]),
    run_sort(Algo, [-4, -4, -7, -15, -9, -4], [-15, -9, -7, -4, -4, -4]),
    run_sort(Algo, [-9999, 9999, 0, -1, 1], [-9999, -1, 0, 1, 9999]),
    run_sort(Algo, [9999, -9999, 9999, -9999], [-9999, -9999, 9999, 9999]),
    run_sort(Algo, [100000, 200000, 100000], [100000, 100000, 200000]),
    run_sort(Algo, [5, 5, 5, 3, 3, 3, 9, 9, 9], [3, 3, 3, 5, 5, 5, 9, 9, 9]).

:- end_tests(sort_tests).
:- begin_tests(get_order_tests).

% Test with empty list should return 0
test(empty_list) :- 
    algorithms:get_order([], 5, 0).

% Test with one element greater should return 1
test(one_element_greater) :- 
    algorithms:get_order([4], 5, 1).

% Test with one element smaller should return 0
test(one_element_smaller) :- 
    algorithms:get_order([6], 5, 0).

% Test with multiple elements, mixed
test(multiple_elements_mixed) :- 
    algorithms:get_order([1, 2, 3, 4, 6, 7, 8], 5, 4).

% Test with multiple elements, all greater
test(multiple_elements_all_greater) :- 
    algorithms:get_order([6, 7, 8, 9], 5, 0).

% Test with multiple elements, all smaller
test(multiple_elements_all_smaller) :- 
    algorithms:get_order([1, 2, 3, 4], 5, 4).

% Test with duplicate elements
test(duplicate_elements) :- 
    algorithms:get_order([1, 2, 2, 3, 4], 3, 3).
test(duplicate_elements_2) :- 
    algorithms:get_order([1, 3, 3, 3, 4, 3], 3, 1).
  
:- end_tests(get_order_tests).
:- begin_tests(get_count).

test(get_counts):-
    algorithms:get_count([2, 1, 1, 3, -1], [2, 1, 1, 3, -1], [3, 1, 1, 4, 0]),
    algorithms:get_count([], [], []),
    algorithms:get_count([1], [1], [0]),
    algorithms:get_count([1, 2, 3], [1, 2, 3], [0, 1, 2]).

test("get count on empty list") :-
    algorithms:get_count([], [], []), !.

test("get count on a list of integers") :-
    algorithms:get_count([3, 1, 2, 4, 0], [3, 1, 2, 4, 0], [3, 1, 2, 4, 0]), !.

test("get count on a list with negative integers") :-
    algorithms:get_count([-2, 0, 3, -1], [-2, 0, 3, -1], [0, 2, 3, 1]), !.

test("get count with repeated elements") :-
    algorithms:get_count([2, 2, 1, 3], [2, 2, 1, 3], [1, 1, 0, 3]), !.

test("get count with unordered list") :-
    algorithms:get_count([2, 3, 1], [2, 3, 1], [1, 2, 0]), !.

test("get count with all the same elements") :-
    algorithms:get_count([3, 3, 3, 3], [3, 3, 3, 3], [0, 0, 0, 0]), !.

:- end_tests(get_count).
:- begin_tests(generate_sorted_list).

test("generate sorted list from empty lists") :-
    algorithms:generate_sorted_list([], [], []), !.

test("generate sorted list from integer lists") :-
    algorithms:generate_sorted_list([4, 3, 2, 1, 0], [0, 1, 2, 3, 4], [4, 3, 2, 1, 0]), !.

test("generate sorted list with negative integers") :-
    algorithms:generate_sorted_list([0, 1, 2, 3], [-2, -1, 0, 3], [-2, -1, 0, 3]), !.

test("generate sorted list with repeated elements") :-
    algorithms:generate_sorted_list([0, 0, 2, 3], [1, 1, 2, 3], [1, 1, 2, 3]), !.

test("generate sorted list with unordered initial list") :-
    algorithms:generate_sorted_list([1, 2, 0], [2, 3, 1], [1, 2, 3]), !.

test("generate sorted list with all the same elements") :-
    algorithms:generate_sorted_list([0, 0, 0, 0], [4, 4, 4, 4], [4, 4, 4, 4]), !.
:- end_tests(generate_sorted_list).

% Run the tests
:- run_tests.
