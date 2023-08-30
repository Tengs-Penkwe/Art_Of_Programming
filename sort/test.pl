% :- use_module('./algorithms.pl').
% :- consult('algorithms.pl').
:- [algorithms].

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
:- begin_tests(counting_sort_tests).

% Tests for get_count/3

% Test counting_sort function with a list of unique elements
test(counting_sort_unique) :-
    counting_sort([4, 1, 3, 2, 5], Sorted), !,
    Sorted = [1, 2, 3, 4, 5].

% Test counting_sort function with an empty list
test(counting_sort_empty) :-
    counting_sort([], Sorted), !,
    Sorted = [].

% Test counting_sort function with duplicates
test(counting_sort_duplicates) :-
    counting_sort([4, 4, 3, 1, 2, 2], Sorted), !,
    Sorted = [1, 2, 2, 3, 4, 4].

% Test counting_sort function with a single element
test(counting_sort_single_element) :-
    counting_sort([1], Sorted), !,
    Sorted = [1].

% Test counting_sort function with two elements
test(counting_sort_two_elements) :-
    counting_sort([2, 1], Sorted), !,
    Sorted = [1, 2].

test(counting_sort_negatives) :-
    counting_sort([-3, -1, -4, -2], Sorted),!,
    Sorted = [-4, -3, -2, -1].

test(counting_sort_mixed_neg_pos) :-
    counting_sort([3, -1, 4, -2, 0], Sorted),!,
    Sorted = [-2, -1, 0, 3, 4].

test(counting_sort_repeating) :-
    counting_sort([1, 1, 1, 1, 1], Sorted),!,
    Sorted = [1, 1, 1, 1, 1].

test(counting_sort_large_numbers) :-
    counting_sort([10000, 20000, 10000], Sorted),!,
    Sorted = [10000, 10000, 20000].

test(counting_sort_single_duplicate) :-
    counting_sort([5, 5], Sorted),!,
    Sorted = [5, 5].

test(counting_sort_all_same_neg) :-
    counting_sort([-2, -2, -2], Sorted),!,
    Sorted = [-2, -2, -2].

test(counting_sort_with_zero) :-
    counting_sort([0], Sorted),!,
    Sorted = [0].

test(counting_sort_descending) :-
    counting_sort([5, 4, 3, 2, 1], Sorted),!,
    Sorted = [1, 2, 3, 4, 5].

test(counting_sort_multiple_duplicates) :-
    counting_sort([3, 3, 2, 2, 1], Sorted),!,
    Sorted = [1, 2, 2, 3, 3].

test(counting_sort_large_list) :-
    counting_sort([1,5,3,2,4,1,5,3,2,4,1,5,3,2,4], Sorted),!,
    Sorted = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5].

test(counting_sort_large_range) :-
    counting_sort([100, 200, 300, 100, 200, 300], Sorted),!,
    Sorted = [100, 100, 200, 200, 300, 300].

test(counting_sort_random_order) :-
    counting_sort([23, 1, 45, 67, 89, 11, 8, 109, 21, 0], Sorted),!,
    Sorted = [0, 1, 8, 11, 21, 23, 45, 67, 89, 109].

test(counting_sort_multiple_zeros) :-
    counting_sort([0, 0, 0, 0, 0, 1], Sorted),!,
    Sorted = [0, 0, 0, 0, 0, 1].

test(counting_sort_negative_and_zero) :-
    counting_sort([0, -1, -1, 0, 0, -1], Sorted),!,
    Sorted = [-1, -1, -1, 0, 0, 0].

test(counting_sort_all_negative) :-
    counting_sort([-4, -4, -7, -15, -9, -4], Sorted),!,
    Sorted = [-15, -9, -7, -4, -4, -4].

test(counting_sort_largest_smallest) :-
    counting_sort([-9999, 9999, 0, -1, 1], Sorted),!,
    Sorted = [-9999, -1, 0, 1, 9999].

test(counting_sort_large_small_combinations) :-
    counting_sort([9999, -9999, 9999, -9999], Sorted),!,
    Sorted = [-9999, -9999, 9999, 9999].

test(counting_sort_multiple_large_numbers) :-
    counting_sort([100000, 200000, 100000], Sorted),!,
    Sorted = [100000, 100000, 200000].

test(counting_sort_multiple_replicates) :-
    counting_sort([5, 5, 5, 3, 3, 3, 9, 9, 9], Sorted),!,
    Sorted = [3, 3, 3, 5, 5, 5, 9, 9, 9].
:- end_tests(counting_sort_tests).

% Run the tests
:- run_tests.
