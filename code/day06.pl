
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(apply)).

:- ['code/aoc2020.pl'].

test01(X):-
    solve_day06a('code/data/day06_test', X).

% ?- test01(X).
% X = 11.

test02(X):-
    solve_day06a('code/data/day06_input', X).

% ?- test02(X).
% X = 6531 .

solve_day06a(FileName, Out):-
    get_input_as_list_of_lists(FileName, InputStrings),
    maplist(string_list_to_char_list_list, InputStrings, CharListList),
    maplist(flatten, CharListList, CharLists),
    maplist(list_to_set, CharLists, CharSets),
    maplist(length, CharSets, Ls),
    sum_list(Ls, Out).


% get_input_as_list_of_lists('code/data/day06_test', Out).
% Out = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a", "a"], ["b"]] .
get_input_as_list_of_lists(FileName, Out):-
    read_file_to_string(FileName, InputString, []),
    re_split('\\n\\n', InputString, [Str1|Rest]),
    every_nth_member(2, Rest, Strs),
    length([Str1|Strs], L),
    list_of_n_items(L, '\n', Arg2),
    list_of_n_items(L, '', Arg3),
    maplist(split_string, [Str1|Strs], Arg2, Arg3, Out).
    
% ?- string_list_to_char_list_list(["ab", "ac"], Out).
% Out = [[a, b], [a, c]].
string_list_to_char_list_list(StringList, CharListList):-
    maplist(string_chars, StringList, CharListList).

%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%

test03(X):-
    solve_day06b('code/data/day06_test', X).

% ?- test03(X).
% X = 6 ;

test04(X):-
    solve_day06b('code/data/day06_input', X).

% For this one, need to find which members
% are in all the sets:

% ?- test04(X).
% X = 3143 ;

solve_day06b(FileName, Out):-
    get_input_as_list_of_lists(FileName, InputStrings),
    maplist(string_list_to_char_list_list, InputStrings, CharListList),
    maplist(members_of_all_lists, CharListList, CommonCharLists),
    maplist(length, CommonCharLists, Ls),
    sum_list(Ls, Out).


members_of_all_lists([List], Set):-
    list_to_set(List, Set).
members_of_all_lists([List|Lists], Out):-
    list_to_set(List, Set),
    members_of_all_lists(Lists, CommonMembers),
    intersection(Set, CommonMembers, Out).